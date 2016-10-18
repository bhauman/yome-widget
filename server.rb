require 'sinatra'
require 'mail'

set :logging, true

Mail.defaults do
  delivery_method :smtp, {
    :address => 'smtp.sendgrid.net',
    :port => '587',
    :domain => 'heroku.com',
    :user_name => ENV['SENDGRID_USERNAME'],
    :password => ENV['SENDGRID_PASSWORD'],
    :authentication => :plain,
    :enable_starttls_auto => true
  }
end

helpers do
  def logger
    request.logger
  end

  def send_mail(data)
    Mail.deliver do
      to 'bhauman@gmail.com, redskyshelters@gmail.com'
      from 'bhauman@gmail.com'
      subject 'Yome form filled out'
      body "A New Yome Request!!

We are interested in a Yome!

Name:     #{ data[:name] }
Email:    #{ data[:email] }
City:     #{ data[:city] }
ZIP Code: #{ data[:zip] }
Comments:
#{ data[:comments] }

You can view the yome here:
http://redskyshelters.com/calculator/#!/yome/#{ data[:code] } 

Enjoy!!!"
    end    
  end
end

post '/mail/deets' do
  content_type :json
  logger.info params
  send_mail params
end
