// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using AutoDecorated;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

#nullable enable

namespace CSharpSourceGeneratorSamples
{
    [Generator]
    public class AutoDecoratorGenerator : ISourceGenerator
    {
        public void Initialize(GeneratorInitializationContext context)
        {
            context.RegisterForSyntaxNotifications(() => new DecoratedAttributeFieldsSyntaxReceiver());
        }

        public void Execute(GeneratorExecutionContext context)
        {
            if (context.SyntaxContextReceiver is not DecoratedAttributeFieldsSyntaxReceiver receiver || receiver.Field is null)
                return;

            // No such attribute?
            if (context.Compilation.GetTypeByMetadataName(typeof(Decorate).FullName) is not INamespaceOrTypeSymbol decorateSymbol)
                return;

            // No such attribute?
            if (context.Compilation.GetTypeByMetadataName(typeof(Override).FullName) is not INamespaceOrTypeSymbol overrideSymbol)
                return;

            IFieldSymbol fieldSymbol = receiver.Field;

            // group the fields by class, and generate the source
            string classSource = GeneratePartialClassSource(fieldSymbol.ContainingType, fieldSymbol, decorateSymbol, overrideSymbol, context);
            context.AddSource($"{fieldSymbol.ContainingType.Name}_AutoDecorated.cs", SourceText.From(classSource, Encoding.UTF8));
        }

        private string GeneratePartialClassSource(INamedTypeSymbol classSymbol, IFieldSymbol fieldSymbol, ISymbol decorateSymbol, ISymbol overrideSymbol, GeneratorExecutionContext context)
        {
            // TODO: find a way to issue proper diagnostics
            
            if (!classSymbol.ContainingSymbol.Equals(classSymbol.ContainingNamespace, SymbolEqualityComparer.Default))
            {
                return ""; //TODO: issue a diagnostic that it must be top level
            }

            string namespaceName = classSymbol.ContainingNamespace.ToDisplayString();

            // Get methods and properties with OverrideSymbol

            ImmutableArray<ISymbol> decoratedClassMembers = fieldSymbol.Type.GetMembers(); // GetInterfaces ? Etc

            ImmutableArray<INamedTypeSymbol> decoratedInterfaces = ImmutableArray<INamedTypeSymbol>.Empty;//fieldSymbol.Type.Interfaces;
            
            ISymbol[] overridenMembers =
                decoratedClassMembers
                   .AsEnumerable()
                   .Where(member => member.GetAttributes()
                                          .AsEnumerable()
                                          .Any(attribute => attribute.AttributeClass?.Equals(overrideSymbol, SymbolEqualityComparer.Default) ?? false))
                   .ToArray();
            
            // Check for validity

            ISymbol[] validOverridenMembers =
                overridenMembers
                   .Choose(member =>
                    {
                        if (OverridenMethodHasMatchingMethodInDecorated(member, decoratedClassMembers)
                        || OverridenMethodHasMatchingPropertyInDecorated(member, decoratedClassMembers))
                            return (true, member);
                        return default;
                    })
                   .ToArray();
            
            
            // Now get members to Generate
            
            ISymbol[] membersToGenerate =
                decoratedClassMembers
                   .AsEnumerable()
                   .Where(member => validOverridenMembers.Any(overriden => !(MatchMembersAsProperties(member, overriden)
                                                                          || MatchMembersAsMethods(member, overriden))))
                   .ToArray();

            StringBuilder source = new($@"
namespace {namespaceName}
{{
    public partial class {classSymbol.Name} : {string.Join(", ", decoratedInterfaces.Select(decoratedInterface => decoratedInterface.Name))}
    {{
");
            
            // Create members
            foreach (ISymbol memberSymbol in membersToGenerate)
            {
                ProcessMember(source, fieldSymbol, memberSymbol);
            }

            source.Append("} }");
            return source.ToString();
        }

        private static void ProcessMember(StringBuilder source, IFieldSymbol fieldSymbol, ISymbol memberSymbol)
        {
            string fieldName = fieldSymbol.Name;
            
            switch (memberSymbol)
            {
                case IMethodSymbol method:
                    string methodName = method.Name;
                    string returnTypeName = method.ReturnType.Name;
                    string typeParametersString = method.TypeParameters.Length > 0 ? $"<{string.Join(", ", method.TypeParameters.Select(typeParameter => typeParameter.Name))}>" : "";
                    string parametersString = string.Join(", ", method.Parameters.Select(parameter => parameter.Name));

                    source.AppendLine($"public {returnTypeName} {methodName}{typeParametersString} ({parametersString}) => {fieldName}.{methodName}({parametersString})");
                    break;
                case IPropertySymbol property:

                    string propertyName = property.Name;
                    string typeName = property.Type.Name;

                    source.AppendLine($"public {typeName} {propertyName} => {fieldName}.{propertyName}");
                    
                    break;
            }
        }

        private static bool MatchMembersAsProperties(ISymbol fromDecorated, ISymbol fromOverriden)
        {
            if (fromDecorated is not IPropertySymbol decoratedProp
             || fromOverriden is not IPropertySymbol overridenProp)
                return false;

            if (!decoratedProp.Name.Equals(overridenProp.Name, StringComparison.Ordinal))
                return false;

            if (!decoratedProp.Type.Equals(overridenProp.Type, SymbolEqualityComparer.Default))
                return false;
            //context.ReportDiagnostic(null); // TODO: Diagnostics

            return true;
        }

        private static bool MatchMembersAsMethods(ISymbol fromDecorated, ISymbol fromOverriden)
        {
            if (fromDecorated is not IMethodSymbol decoratedMethod
             || fromOverriden is not IMethodSymbol overridenMethod)
                return false;

            if (!decoratedMethod.Name.Equals(overridenMethod.Name, StringComparison.Ordinal))
                return false;

            if (!decoratedMethod.ReturnType.Equals(overridenMethod.ReturnType, SymbolEqualityComparer.Default))
                return false;

            return decoratedMethod.Parameters.SequenceEqual(overridenMethod.Parameters);
        }

        private static bool OverridenMethodHasMatchingMethodInDecorated(ISymbol fromDecorated, IEnumerable<ISymbol> fromOverriden) => 
            fromOverriden.Any(symbol => MatchMembersAsMethods(fromDecorated, symbol));

        private static bool OverridenMethodHasMatchingPropertyInDecorated(ISymbol fromDecorated, IEnumerable<ISymbol> fromOverriden) => 
            fromOverriden.Any(symbol => MatchMembersAsProperties(fromDecorated, symbol));
    }

    /// <summary>
    /// Created on demand before each generation pass
    /// </summary>
    internal class DecoratedAttributeFieldsSyntaxReceiver : ISyntaxContextReceiver
    {
        public IFieldSymbol? Field { get; private set; }

        /// <summary>
        /// Called for every syntax node in the compilation, we can inspect the nodes and save any information useful for generation
        /// </summary>
        public void OnVisitSyntaxNode(GeneratorSyntaxContext context)
        {
            // any field with at least one attribute is a candidate for property generation
            if (context.Node is FieldDeclarationSyntax { AttributeLists: { Count: > 0 } } fieldDeclarationSyntax)
            {
                foreach (VariableDeclaratorSyntax variable in fieldDeclarationSyntax.Declaration.Variables)
                {
                    // Get the symbol being declared by the field, and keep it if its annotated
                    if (context.SemanticModel.GetDeclaredSymbol(variable) is IFieldSymbol fieldSymbol
                     && fieldSymbol.GetAttributes().Any(ad => ad.AttributeClass?.ToDisplayString() == typeof(Decorate).FullName))
                    {
                        Field = fieldSymbol;
                        break;
                    }
                }
            }
        }
    }
}
