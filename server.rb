require 'sinatra'

set :logging, true

helpers do
  def logger
    request.logger
  end
end

post '/mail/deets' do
  content_type :json
  logger.info params
end
