# TODO: Create task for configuring pre-commit hook(s).
#       We should make sure that any `created_at:' and/or `updated_at:' YAML
#         front-matter are correct.

require 'uuid'

desc "Makes a new post"
task :mkpost do
  uuid = UUID.new.generate(:compact)
  date = Time.now.strftime("%Y-%m-%d")

  File.open("_posts/#{date}-name.textile", "w") do |f|
    f << <<-eopost
---
layout: post
uuid: #{uuid}
title: 
---

  eopost

  end
end