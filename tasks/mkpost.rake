desc "Make a new post"
task :mkpost do
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
