require 'uuid'

desc "Makes a new post"
task :mkpost do
  uuid = UUID.new.generate(:compact)
  short_date = Time.now.strftime("%Y-%m-%d")
  long_date = Time.now.strftime("%Y-%m-%d %H:%M:%S")

  file_name = "_posts/#{short_date}-name.textile"
  puts "Generated #{file_name}"
  File.open(file_name, "w") do |f|
    f << <<-EOF
---
layout: post
uuid: #{uuid}
date: #{long_date}
comments: true
title: 
---

  EOF

  end
end