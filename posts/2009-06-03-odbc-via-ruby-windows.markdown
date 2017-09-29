---
title: ODBC via Ruby on Windows
tags: ruby
---

I *just* got Ruby to connect to a database via ODBC and the
[dbi](https://rubygems.org/gems/dbi) gem (particularly, MSSQL). I ran
into a few problems along the way, so here are some of my notes.

# DBI

Make sure you have the dbi and dbd-odbc gems installed:

``` {.console}
gem install dbi 
gem install dbd-odbc
```

Next, you'll want to make sure that you have the <code>odbc.so</code>
and <code>odbc\_utf8.so</code> files in your Ruby installation (check
your <code>lib\\ruby\\1.8\\i386-mingw32</code> folder). If you do not
have these, go grab <code>i386-msvcrt-ruby-odbc.zip</code> from here:

<http://www.ch-werner.de/rubyodbc/>

You can now use dbi like so:

```ruby
require 'rubygems'
require 'dbi'

DBI.connect('DBI:ODBC:test_dsn') do | dbh |
  dbh.select_all('select * from sys.tables') do | row |
    p row
  end
end
```

# ActiveRecord

You can also use <code>active\_record</code>. First, go grab
<code>odbc-rails</code> from here:

http://odbc-rails.rubyforge.org/

If you go to the download page, you'll see that there are two versions:
1.5 and 2.0 for Rails 1.x and 2.x, respectively. Doing a gem install
odbc-rails resulted in 1.5 being installed on my machine, so I
downloaded the
"activerecord-odbc-adapter-2.0.gem"http://rubyforge.org/frs/download.php/35985/activerecord-odbc-adapter-2.0.gem
file from rubyforge and installed that. Once you've installed the gem,
you can follow the instructions on the odbc-rails homepage to use it
from Rails, or you can use some code like the following if you want to
use active\_record outside of a Rails project:

```ruby
require 'rubygems'
require 'active_record'

ActiveRecord::Base.establish_connection(
  :adapter => "odbc",
  :dsn => "test")

class Person < ActiveRecord::Base
end

p Person.find(:all)
```

**Note:** If you're connecting specifically to SQL Server, it's probably
a better idea to use the
[activerecord-sqlserver-adapter](https://rubygems.org/gems/activerecord-sqlserver-adapter)
gem. Here's a good link on that topic:

<http://rubyrailsandwindows.blogspot.com/2008/03/rails-2-and-sql-server-2008-on-windows_24.html>

# Alternatives

There are also a few other alternatives. Check these out:

-   <http://rubyonwindows.blogspot.com/2007/09/using-ruby-sql-dmo-to-automate-sql.html>
-   <http://rubyonwindows.blogspot.com/2007/03/ruby-ado-and-sqlser>
