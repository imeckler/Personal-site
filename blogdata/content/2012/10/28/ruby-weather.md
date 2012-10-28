| title: A weather utility in Ruby
| author: Izaak Meckler <izaakmeckler@me.com>
| published: 2012-10-28T13:31:46-0500
| updated: 2012-10-28T13:31:46-0500

I'd never really given Ruby a proper chance, so this past Friday I decided to write a simple weather utility as a way to get my feet wet with the lanugage and the standard library. When run, the program should print information about today's weather, as well as the current weather. I used the [World Weather Online API](http://www.worldweatheronline.com). Let's get to the code!

First, a few `require`s:

<pre class="prettyprint lang-ruby">
require 'net/http'
require 'socket'
require 'json'
</pre>

Since the API supports querying by IP address (which is then matched with an approximate location on their end), I wrote a little function to get the computer's current public IP.

<pre class="prettyprint lang-ruby">
def get_public_ip
  Socket.ip_address_list.last.ip_address
end
</pre>

Next, we'll have to query the API, falling back on querying by zip code if the IP address query fails.

<pre class="prettyprint lang-ruby">
def get_weather
  ip = get_public_ip
  url = "http://free.worldweatheronline.com/feed/weather.ashx?q=#{ip}&format=json&num_of_days=1&key=#{APIKEY}"
  weather = (JSON.load Net::HTTP.get URI(url))['data']

  if weather.has_key? 'error'
    weather = (JSON.load Net::HTTP.get URI('http://free.worldweatheronline.com/feed/weather.ashx?q=60616&format=json&num_of_days=1&key=#{APIKEY}'))['data']
  end
  weather
end
</pre>

I'm not really sure how to deal with those long ugly lines. Anyway, the nicer code starts now. Given the output of `get_weather`, we'll build a hash that maps a label to the corresponding piece of information for both the day's weather and the current weather. The JSON returned by the API has slightly different information and keys for each, so I wrote two different functions.

<pre class="prettyprint lang-ruby">
def current_hash(weather)
  { 'Description' => weather['weatherDesc'].first['value'],
    'Cloud cover' => weather['cloudcover'],
    'Humidity'    => weather['humidity'],
    'Temperature' => weather['temp_F'],
    'Wind speed'  => weather['windspeedMiles']
  }
end

def today_hash(weather)
  { 'Description' => weather['weatherDesc'].first['value'],
    'Humidity'    => weather['humidity'],
    'Temperature' => "#{weather['tempMinF']} - #{weather['tempMaxF']}",
    'Wind speed'  => weather['windspeedMiles']
  }
end
</pre>

I want the output to look something like this
<pre class="prettyprint">
           Description    Humidity  Temperature  Wind speed  Cloud cover
Today      Overcast                 42 - 48      15                     
Currently  Partly Cloudy  46        48           17          75         
</pre>

so the keys in the hashes correspond to column labels and each of the hashes corresponds to a row. So, we monkey-patch `Hash` with a combine method, which combines two hashes by placing the values at corresponding keys into a list.

<pre class="prettyprint lang-ruby">
class Hash
  def combine(hash_2)
    new_hash = {}

    self.each { |k, v| new_hash[k] = [v] }

    hash_2.each do |k, v|
      new_hash[k] ||= [nil]
      new_hash[k] &lt;&lt; v
    end

    new_hash
  end
end
</pre>

Now we can call combine on the hash containing today's weather, pass it the hash containing the current weather, and end up with a hash that maps from labels to lists of two pieces of information, one from each of the source hashes. Essentially the exact representation we want for conversion to a string.

Putting it all together, here's the `weather_string` function
<pre class="prettyprint lang-ruby">
def weather_string
  weather = get_weather
  today_weather = today_hash weather['weather'].first
  current_weather = current_hash weather['current_condition'].first

  weather = today_weather.combine current_weather

  label_column = ['         ', 'Today    ', 'Currently']

  columns = [label_column] + weather.map do |label, info|
    column = ([label] + info).map &:to_s
    width = column.map(&:length).max
    column.map {|s| s + ' ' * (width - s.length)}
  end

  (columns.transpose.map {|r| r.join '  '}).join "\n"
end
</pre>

The function retrieves the weather and builds the two hashes we need, then combines them into a single mapping from column labels to lists of information. Then it constructs a list of lists of strings, `columns`, where each element is of the form `[LABEL, TODAYINFO, CURRINFO]`, padded out with enough spaces so that each item is the same length. Finally, we transpose `columns` to get a list of rows, join the elements of each row together with two spaces, then join all the rows together with newlines. All in all, a good exercise for learning the language and a few libraries. I added a call to the script to my `.profile`, so I'm greeted with the weather whenever I open my shell.