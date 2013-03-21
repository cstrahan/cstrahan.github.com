task :shelve do
  committed = `git ls-tree --name-only -r HEAD`.split("\n")
  uncommitted = Dir.glob("_posts/*") - committed
  uncommitted.each do |post|
    name = File.basename(post)
    mv post, File.join("drafts", name)
  end
end

task :unshelve do
  drafts = Dir.glob("drafts/*")
  drafts.each do |draft|
    name = File.basename(draft)
    mv draft, File.join("_posts", name)
  end
end
