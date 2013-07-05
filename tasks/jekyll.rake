desc 'start jekyll'
task :jekyll do
  # system "open http://localhost:4000"
  sh "bundle exec jekyll --server --auto"
end
