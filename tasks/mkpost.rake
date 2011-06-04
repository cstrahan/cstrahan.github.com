require 'uuid'

desc "Makes a new post"
task :mkpost do
  uuid = UUID.new.generate(:compact)
  short_date = Time.now.strftime("%Y-%m-%d")
  long_date = Time.now.strftime("%Y-%m-%d %H:%M:%S")

  File.open("_posts/#{short_date}-name.textile", "w") do |f|
    f << <<-eopost
---
layout: post
uuid: #{uuid}
date: #{long_date}
comments: true
title: 
---

  eopost

  end
end