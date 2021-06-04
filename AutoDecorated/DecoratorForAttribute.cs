// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;

namespace AutoDecorated
{
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct)]
    public class DecoratorForAttribute : Attribute
    {
        public DecoratorForAttribute(Type type)
        {
            Type = type;
        }

        public Type Type { get; }
    }
}
