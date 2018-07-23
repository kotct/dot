lib_directory = File.expand_path('lib', File.dirname(__FILE__))
$LOAD_PATH.unshift(lib_directory) unless $LOAD_PATH.include?(lib_directory)

require 'fileutils'

RAKEFILES = Dir.glob(File.expand_path(File.join('*', 'Rakefile'), File.dirname(__FILE__)), File::FNM_DOTMATCH)

RAKEFILES.each do |rakefile|
	if !FileUtils.identical?(rakefile, __FILE__)
		load rakefile
	end
end

desc 'Runs the install task from all sub-Rakefiles if they have it'
task :install do
	tasks = Rake.application.tasks
	tasks.select do |task|
		task.name =~ /\:install/
	end.each do |t|
		puts "Executing task: #{t.name.inspect}"
		t.execute
	end
end
