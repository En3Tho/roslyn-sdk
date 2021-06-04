// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;

namespace AutoDecorated
{
    /// <summary>
    /// Indicates that a Method or a Property with such name and signature is manually overriden and should not be auto generated.
    /// </summary>
    [AttributeUsage(AttributeTargets.Property | AttributeTargets.Method)]
    public class Override : Attribute { }
}
