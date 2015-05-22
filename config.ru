require "rubygems"
require "rack"
require "rack/cors"
require "./server.rb"

use Rack::Cors do
  allow do
    origins '*'
    resource '/mail/deets', :headers => :any, :methods => :get
  end
end

run Sinatra::Application
