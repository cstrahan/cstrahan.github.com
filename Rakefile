require 'uuid'

PROJECT_ROOT = File.expand_path(File.dirname(__FILE__))
TASKS_DIR = File.join(PROJECT_ROOT, "tasks")
TASKS = Dir.glob(File.join(TASKS_DIR, "*.rake"))

$:.unshift(TASKS_DIR) unless $:.include?(TASKS_DIR)

def uuid
  UUID.new.generate(:compact)
end

def short_date(time=Time.now)
  time.strftime("%Y-%m-%d")
end

def long_date(time=Time.now)
  time.strftime("%Y-%m-%d %H:%M:%S")
end

def parse_long_date(str)
  DateTime.strptime(str, "%Y-%m-%d %H:%M:%S")
end

TASKS.each do |task|
  load task
end
