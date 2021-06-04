namespace AutoDecoratedGeneratorBackend

open System
open System.Linq
open AutoDecorated
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp.Syntax

[<AutoOpen>]
module Core =
    let inline (^) f x = f x

module Seq =
    let inline ofType<'a> seq = Enumerable.OfType<'a> seq

type GenerationError =
    | OverridenMethodIsNotFound of Method: IMethodSymbol
    | OverridenPropertyIsNotFound of Property: IPropertySymbol
    | GenericArityMismatch of Overriden: IMethodSymbol * Decorated: IMethodSymbol
    | PropertyIsAlreadyImplemented of Property: IPropertySymbol
    | MethodIsAlreadyImplemented of Method: IMethodSymbol

type GeneratedSource = string    

type GenerationResult = Result<GeneratedSource, GenerationError>

module Symbol =
    let (|SameAs|_|) symbol2 (symbol1: ISymbol) =
        if symbol1.Equals(symbol2, SymbolEqualityComparer.Default) then Some() else None
    
    let (|SameAsNullable|_|) symbol2 (symbol1: ISymbol) =
        if symbol1.Equals(symbol2, SymbolEqualityComparer.IncludeNullability) then Some() else None
        
    let (|HasAttribute|_|) attributeName (symbol: ISymbol) =
        symbol.GetAttributes()
        |> Seq.tryFind ^ fun attribute -> attribute.AttributeClass.ToDisplayParts() = attributeName
    
module Symbols =
    let areSame (symbol1: ISymbol) symbol2 = symbol1.Equals(symbol2, SymbolEqualityComparer.Default)
    let areSameIncludeNullability (symbol1: ISymbol) symbol2 = symbol1.Equals(symbol2, SymbolEqualityComparer.IncludeNullability)
    
module SymbolEquality =
    
    let matchAsProperties (decorated: ISymbol) (overriden: ISymbol) =
        match decorated, overriden with
        | :? IPropertySymbol as decorated, (:? IPropertySymbol as overriden) ->
            decorated.Name.Equals(overriden.Name, StringComparison.Ordinal)
            && Symbols.areSame decorated.Type overriden.Type 
        | _ -> false
        
    let matchAsMethods (decorated: ISymbol) (overriden: ISymbol) =
        match decorated, overriden with
        | :? IMethodSymbol as decorated, (:? IMethodSymbol as overriden) ->
            decorated.Name.Equals(overriden.Name, StringComparison.Ordinal)
            && Symbols.areSame decorated.ReturnType overriden.ReturnType
            && decorated.Parameters.SequenceEqual(overriden.Parameters)
        | _ -> false

type DecoratedAttributeFieldsSyntaxReceiver() =
    let mutable fieldSymbol = null
    
    let (|ContainsDecoratedField|_|) (context: GeneratorSyntaxContext) (syntax: FieldDeclarationSyntax) =
        if syntax.AttributeLists.Count = 0 then None
        else
            syntax.Declaration.Variables
            |> Seq.map context.SemanticModel.GetDeclaredSymbol
            |> Seq.ofType<IFieldSymbol>
            |> Seq.tryFind ^ fun field ->           
                field.GetAttributes()
                |> Seq.exists ^ fun ad ->
                    ad.AttributeClass <> null
                    && ad.AttributeClass.ToDisplayString() = typeof<Decorate>.FullName
                
    let onVisitSyntaxNode (context: GeneratorSyntaxContext) =
        match context.Node with
        | :? FieldDeclarationSyntax as syntax ->
            match syntax with
            | ContainsDecoratedField context field ->
                fieldSymbol <- field
            | _ -> ()
        | _ -> ()
       
module AutoDecoratedGenerator =
    let initialize (context: GeneratorInitializationContext) = ()
    let execute (context: GeneratorExecutionContext) = ()