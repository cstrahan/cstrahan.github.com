PROJECT_ROOT = File.expand_path(File.dirname(__FILE__))
TASKS_DIR = File.join(PROJECT_ROOT, "tasks")
TASKS = Dir.glob(File.join(TASKS_DIR, "*.rake"))

$:.unshift(TASKS_DIR) unless $:.include?(TASKS_DIR)

TASKS.each do |task|
  load task
end