# /^date: \d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}/
task :rename do
  Dir.glob("_posts/*").each do |old_path|
    date = File.readlines(old_path).detect {|line| line.start_with?("date: ")}[6..15]
    new_path = old_path.sub(/\d{4}-\d{2}-\d{2}/, date)

    if old_path != new_path
      mv old_path, new_path
    end
  end
end
