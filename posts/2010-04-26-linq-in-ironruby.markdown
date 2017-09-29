---
title: Linq in IronRuby
tags: ruby
---

Perusing through some old IronRuby mailing lists, I saw some people
inquiring about using Linq from IronRuby. The general response was that
it wasn't necessary - you can use the equivalent methods on the
Enumerable module. That works fine if your working with an IEnumerable,
but there's one big problem - you can't consume an API that exposes an
IQueryable without calling \#to\_a. That sucks because you're now
eager-loading everything into RAM. C\# devs have the benefit of lambda
expressions, but there isn't an equivelent in Ruby. You could create an
Expression tree manually, but that sucks even harder.

That's way to much suckage going on - now lets make it blow! Er, suck
less:

``` ruby
require 'System.Core, Version=3.5.0.0, Culture=Neutral, PublicKeyToken=b77a5c561934e089'  
require 'System.Linq.Dynamic'  
  
# Add DynamicQueryable methods to IQueryable[T]  
System::Linq::IQueryable[1].module_eval do  
  dynamic_methods = %w{ any count group_by order_by select skip take where }  
  dynamic_methods.each do |method|  
    define_method("dynamic_#{method}") do |*args|  
      System::Linq::Dynamic::DynamicQueryable.method(method).call(self, *args)  
    end  
  end  
end  
  
# Add some usefull methods to ::Enumerable and IEnumerable  
enumerables = [Enumerable, System::Collections::IEnumerable]  
enumerables.each do |enum_module|  
  enum_module.module_eval do  
    def as_queryable  
      System::Linq::Queryable.as_queryable(self)  
    end  
  
    def cast(type)  
      System::Linq::Enumerable.method(:cast).of(type).call(self)  
    end  
  end  
end  
  
# Check it out!  
nums = [1,2,3,4,5,6,7,8,9]  
p nums.cast(Fixnum) \  
      .as_queryable \  
      .dynamic_where("it % @0 == @1", 2, 0) \  
      .to_a  #=> [2, 4, 6, 8]
```

# What's going on here?

First off, I'm using the DynamicQueryable class that comes packaged as
an example solution with Visual Studio 2008. I compiled the project as a
class library, output as System.Linq.Dynamic.dll (thus the `require
'System.Linq.Dynamic'`).

Next, you'll see that I'm dynamically defining methods on
`IQueryable<T>` that delegate to
`System.Linq.Queryable`. I create a list of the Queryable
static method names that I want to include, loop over each, and use
`Module#define_method` to create the method.

Lastly, I add `#as_queryable` and `#cast`
methods to `::Enumerable` and `IEnumerable`, just
for convenience.

It's not quite as nice as C\#, but I'm sure it could get there with some
lovin'. Next on my “todo” list is to write an example of using this
technique with Linq-to-SQL, with some metaprogramming sprinkled on top.
I'll present a couple things here in a little while:

1.  Write a DSL for Linq-to-SQL mappings.
2.  Get active_record-esque ease of use with Linq-to-SQL. Just load a
    DBML file - no need for code generation.
3.  Even better active_record-ness. Same as above, with the benefit of
    using just a connection string.
