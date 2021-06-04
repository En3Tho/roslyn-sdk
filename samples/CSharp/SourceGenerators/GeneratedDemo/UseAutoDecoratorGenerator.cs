using System;
using System.Collections.Generic;
using AutoDecorated;

namespace GeneratedDemo
{
    public partial class ConsoleWriteLineDictionaryDecorator<TKey, TValue>
    {
        [Decorate]
        private readonly Dictionary<TKey, TValue> _decorated;

        public ConsoleWriteLineDictionaryDecorator(Dictionary<TKey, TValue> decorated)
        {
            _decorated = decorated;
        }

        [Override]
        public bool TryGetValue(TKey key, out TValue value)
        {
            Console.WriteLine("This is a manual override");
            return _decorated.TryGetValue(key, out value);
        }
        
        [Override]
        public bool ContainsKey(TKey key) => _decorated.ContainsKey(key);

    }

    public static class UseAutoDecoratorGenerator
    {
        public static void Run()
        {
            var decorated = new ConsoleWriteLineDictionaryDecorator<int, int>(null);
            Console.Write(decorated.Count);
        }
    }
}
