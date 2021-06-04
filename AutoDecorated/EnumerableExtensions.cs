// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;

namespace AutoDecorated
{
    public static class EnumerableExtensions
    {
        public static IEnumerable<U> Choose<T, U>(this IEnumerable<T> values, Func<T, (bool, U)> chooser)
        {
            foreach (var value in values)
            {
                var (success, result) = chooser(value);
                if (success) yield return result;
            }
        }
    }
}
