desc "publish the site to s3"
task :push do
  sh 'bundle exec jekyll-s3 --headless'
end
