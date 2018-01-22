import Data.Char
import Data.Maybe
import System.Environment
import System.Exit
import System.Random
-- https://stackoverflow.com/questions/8416365/generate-a-random-integer-in-a-range-in-haskell
import System.IO.Unsafe 
import Control.Monad(when)

--database 
-- entire database for general questions
numList = ["201100","134000","550000","368000","450000","1300","301000","550000","37200000","1500000","27100","450000","246000","1220000","165000","550000","246000","1220000","368000","245000","1000000","4090000","1830000","90500","49500","3350000","368000","90500","1220000","246000","135000","5000000","1500000","823000","8100","673000","1500000","246000","368000","18100","823000","1830000","3350000","165000","11100000","6120000","1830000","550000","823000","1000000","368000","1500000","450000","83100000","823000","301000","673000","368000","33100","301000","1500000","1220000","450000","246000","165000","450000","673000","1830000","226000000","165000","201000","1220000","550000","1000000","1220000","450000","201000","1220000","246000","1000000","450000","301000","18100","550000","673000","1000000","7480000","3350000","2240000","2740000","368000","60500","2240000","1220000","7480000","673000","20400000","90500","550000","49500","2740000","1830000","18100","90500","74000","368000","165000","1220000","450000","1830000","90500","2740000","823000","49500","823000","60500","1000000","1830000","1220000","301000","3350000","2240000","673000","33100","550000","4090000","2240000","1500000","1000000","165000","90500","301000","450000","246000","450000","1000000","6120000","823000","49500","246000","2740000","823000","45500000","550000","74000","368000","1500000","33100","12100","49500","165000","110000","673000","2240000","1830000","1220000","90500","550000","673000","74000","110000","1000000","450000","201000","673000","6120000","33100","246000","1830000","135000","30400000","49500","22000","1220000","90500","301000","1500000","90500","1500000","6120000","550000","4090000","368000","301000","201000","165000","74000","1500000","550000","301000","823000","90500","1220000","823000","673000","1500000","6120000","246000","74000","550000","550000","135000","1220000","40500","673000","7480000","246000","823000","4090000","823000","450000","1220000","65000","201000","550000","33100","74000","301000","301000","550000","14800","4090000","673000","135000","450000","1220000","201000","14800","1500000","49500","1220000","450000","135000","90500","9140000","2740000","1000000","4090000","165000","40500","165000","30400000","550000","110000","165000","2240000","165000","301000","550000","165000","2740000","165000","40500","7480000","1000000","823000","9140000","4090000","12100","246000","1220000","1220000","7480000","550000","201000","5000000","2740000","301000","68000000","18100","90500","201000","2240000","1500000","40500","673000","823000","246000","368000","201000","8100","450000","1500000","4090000","201000","11100000","9140000","368000","135000","13600000","110000","7480000","450000","4090000","450000","823000","550000","450000","20400000","74000","2740000","5000000","823000","18100","49500","246000","12100","2240000","823000","5000000","1220000","13600000","74000","1000000","450000","7480000","201000","301000","74000","11100000","3600","1000000","5000000","135000","9140000","165000","550000","368000","165000","18100","1220000","246000","135000","246000","16600000","368000","1220000","1220000","60500","246000","673000","673000","301000","4400","40500","135000","45500000","74000","1500000","165000","60500","1300","368000","368000","1000000","1500000","368000","33100","165000","368000","1300","823000","1220000","74000","673000","673000","90500","246000","5000000","450000","201000","201000","90500","2240000","1220000","90500","1500000","165000","550000","165000","550000","550000","1000000","450000","1220000","1500000","60500","1220000","1000000","110000","3350000","1000000","2240000","1500000","301000","165000","301000","135000","301000","18100","135000","165000","90500","11100000","20400000","201000","90500","9900","201000","1830000","550000","60500","135000","1830000","5000000","1500000","16600000","368000","49500","2240000","201000","7480000","135000","135000","33100","550000","673000","450000","27100","368000","9900","301000","368000","450000","1220000","165000","823000","18100","27100","673000","550000","165000","3350000","823000","2740000","301000","135000","3600","823000","165000","90500","6120000","2740000","33100","301000","110000","12100","368000","1830000","1220000","1830000","450000","1900","1500000","201000","823000","673000","90500","201000","368000","1220000","1830000","301000","2740000","165000","49500","1220000","5000000","11100000","246000","165000","90500","201000","3350000","165000","135000","1000000","246000","301000","673000","40500","1500000","338000000","49500","1000000","1830000","1500000","301000","5000000","450000","13600000","60500","14800","20400000","12100","9900","110000","301000","1000000","368000","1000000","673000","135000","1500000","450000","450000","550000","1220000","550000","246000","368000","5000000","550000","1220000","4090000","673000","11100000","823000","368000","11100000","135000","90500","2240000","49500","450000","201000","673000","1500000","450000","110000","135000","1000000","90500","1220000","2240000","301000","201000","368000"]
numList2 = [201100,134000,550000,368000,450000,1300,301000,550000,37200000,1500000,27100,450000,246000,1220000,165000,550000,246000,1220000,368000,245000,1000000,4090000,1830000,90500,49500,3350000,368000,90500,1220000,246000,135000,5000000,1500000,823000,8100,673000,1500000,246000,368000,18100,823000,1830000,3350000,165000,11100000,6120000,1830000,550000,823000,1000000,368000,1500000,450000,83100000,823000,301000,673000,368000,33100,301000,1500000,1220000,450000,246000,165000,450000,673000,1830000,226000000,165000,201000,1220000,550000,1000000,1220000,450000,201000,1220000,246000,1000000,450000,301000,18100,550000,673000,1000000,7480000,3350000,2240000,2740000,368000,60500,2240000,1220000,7480000,673000,20400000,90500,550000,49500,2740000,1830000,18100,90500,74000,368000,165000,1220000,450000,1830000,90500,2740000,823000,49500,823000,60500,1000000,1830000,1220000,301000,3350000,2240000,673000,33100,550000,4090000,2240000,1500000,1000000,165000,90500,301000,450000,246000,450000,1000000,6120000,823000,49500,246000,2740000,823000,45500000,550000,74000,368000,1500000,33100,12100,49500,165000,110000,673000,2240000,1830000,1220000,90500,550000,673000,74000,110000,1000000,450000,201000,673000,6120000,33100,246000,1830000,135000,30400000,49500,22000,1220000,90500,301000,1500000,90500,1500000,6120000,550000,4090000,368000,301000,201000,165000,74000,1500000,550000,301000,823000,90500,1220000,823000,673000,1500000,6120000,246000,74000,550000,550000,135000,1220000,40500,673000,7480000,246000,823000,4090000,823000,450000,1220000,65000,201000,550000,33100,74000,301000,301000,550000,14800,4090000,673000,135000,450000,1220000,201000,14800,1500000,49500,1220000,450000,135000,90500,9140000,2740000,1000000,4090000,165000,40500,165000,30400000,550000,110000,165000,2240000,165000,301000,550000,165000,2740000,165000,40500,7480000,1000000,823000,9140000,4090000,12100,246000,1220000,1220000,7480000,550000,201000,5000000,2740000,301000,68000000,18100,90500,201000,2240000,1500000,40500,673000,823000,246000,368000,201000,8100,450000,1500000,4090000,201000,11100000,9140000,368000,135000,13600000,110000,7480000,450000,4090000,450000,823000,550000,450000,20400000,74000,2740000,5000000,823000,18100,49500,246000,12100,2240000,823000,5000000,1220000,13600000,74000,1000000,450000,7480000,201000,301000,74000,11100000,3600,1000000,5000000,135000,9140000,165000,550000,368000,165000,18100,1220000,246000,135000,246000,16600000,368000,1220000,1220000,60500,246000,673000,673000,301000,4400,40500,135000,45500000,74000,1500000,165000,60500,1300,368000,368000,1000000,1500000,368000,33100,165000,368000,1300,823000,1220000,74000,673000,673000,90500,246000,5000000,450000,201000,201000,90500,2240000,1220000,90500,1500000,165000,550000,165000,550000,550000,1000000,450000,1220000,1500000,60500,1220000,1000000,110000,3350000,1000000,2240000,1500000,301000,165000,301000,135000,301000,18100,135000,165000,90500,11100000,20400000,201000,90500,9900,201000,1830000,550000,60500,135000,1830000,5000000,1500000,16600000,368000,49500,2240000,201000,7480000,135000,135000,33100,550000,673000,450000,27100,368000,9900,301000,368000,450000,1220000,165000,823000,18100,27100,673000,550000,165000,3350000,823000,2740000,301000,135000,3600,823000,165000,90500,6120000,2740000,33100,301000,110000,12100,368000,1830000,1220000,1830000,450000,1900,1500000,201000,823000,673000,90500,201000,368000,1220000,1830000,301000,2740000,165000,49500,1220000,5000000,11100000,246000,165000,90500,201000,3350000,165000,135000,1000000,246000,301000,673000,40500,1500000,338000000,49500,1000000,1830000,1500000,301000,5000000,450000,13600000,60500,14800,20400000,12100,9900,110000,301000,1000000,368000,1000000,673000,135000,1500000,450000,450000,550000,1220000,550000,246000,368000,5000000,550000,1220000,4090000,673000,11100000,823000,368000,11100000,135000,90500,2240000,49500,450000,201000,673000,1500000,450000,110000,135000,1000000,90500,1220000,2240000,301000,201000,368000]
searchList = ["General Election","Madden 16","VW Golf","DMT","Spirit Away (Movie)","Volcano Surfing","Uganda","Looney Tunes","IKEA","Israel","Willy Wonka","Muslim","New York Jets","Peaky Blinders (TV)","Oxfam","Easter","Gran Torino (Movie)","The Big Bang Theory","Tampa Bay Buccaneers","Winter Solstice","Chow Chow (Dog)","Barnes and Noble","Christmas","Breast Implants","Trafficing","HP","Black Hole","The Catcher In The Rye","Teletubbies","George Foreman","The Pianist","George Michael","Quran","Dyson","Kata Tjuta (Place)","Karma","Diabetes","Thiery Henry","Requiem For A Dream (Movie)","Plasterer (Profession)","UFO","Popeye","Call of Duty","Antidepressants","Samsung","American Express","Big Brother (TV)","Luis Suarez","Islam","Maylaysia","New York Mets","Las Vegas","Maltese (Dog)","Pokemon GO","Attention Deficit Hyperactivity Disorder","Suffragettes (first wave feminist)","Gladiator","Los Angeles Lakers","Scented Candles","Fleas","Maylasia Airlines","Gold","Limousine","Michael Bay","Rich","Princess Diana","Stonehenge","Justin Timberlake","Translate","Fencing","Wilt Chamberlain","Chicago Bulls","Rod Stewart","Novak Djokovic","Bruce Springsteen","Boeing","Kraft","Mini Cooper","Nits","Venus","Prince Charles","Guinness","Four Tops (Band)","Triple H (WWE)","NWA (Rap group)","Putin","Taylor Swift","Tetris","Breaking Bad","Mickey Mouse","Aretha Franklin","Franz Beckenbauer","Wikileaks","Mark Zuckerberg","The Walking Dead","Pampers","Spotify","Bjorn Borg (Tennis Player)","Hinduism","Wii Sports","Converse","Love Island (TV)","Sky Diving","The Secret Garden (Book)","Britains Got Talent","Breast Cancer","Scuba Diving","Doctors","Marie Curie","Michael Jordan","Mirrors Edge (Video Game)","Valentines Day","Gangnam Style","Willie Mays","Alcohol","Schlinders List","Person Of Interest (TV)","Burberry","Family Guy","Saxophone","Emirates (Airline)","Leicester City","Lionel Richie","Bovril (Yeast Drink)","The New York Times","Adele","New York","Theresa May","Silicon Valley","Cadbury","Poor","Capitalism","Violin","Taipei","Holocaust","Eczema","Flamengo (Soccer team)","Saudi Arabia","Hang Gliding","Carpenter","Timer","Prada","News","Eden Hazard","Owen Hart","San Francisco 49ers","Myspace","Ice Climbing","World Hunger","Uncle Bens (food)","There Will Be Blood (Movie)","John McEnroe (Tennis Player)","Spain","Illuminati","Cartoons","Jesus","Blood Moon","Snowboard","AK-47","The Black Eye Peas","Rock Climbing","Rafael Nadal","Cocaine","Nintendo Wii","ABBA (band)","Fitbit","The Diary of Anne Frank","Benzodiazepines","TED (talks)","Magic Mushrooms","Paypal","Hank Aaron (Baseball Player)","Coachella Festival","Playstation VR","Six Nations","Wes Anderson","Lacoste","Bolton Wanderers (Soccer team)","Boxing","Toyota","Chia Seeds","Batman","Reykjavik","Bupa","Human Rights","Anti Semitism","General Mills","Elvis Presley","Reggae","Inglourious Basterds","Mona Lisa","David and Goliath","Recipes","Scabies","FA Cup","AC Milan","Microsoft","Staffordshire Bull Terrier","The Powerpuff Girls (TV)","Seattle Mariners (Baseball Team)","Swimming","2001 A Space Odyssey","Skrillex","The X Factor","Transgender","Verizon","Doritos","Johnny Cash","Jennifer Lawrence","Cannabis","Jeremy Corbyn","Car Insurance","Rugby World Cup","Harp","Manchester By The Sea","Egyptian Pyramids","Welder","Chinese New Year","Oldboy (Movie)","Moscow Mule","Free Tibet","Audi","Stevie Wonder","Contraception","Steven Spielberg","General Electric (GE)","Dobermann","Potala Palace","Super Bowl","Exam Results","Labrador","Norwich University Hospital","Human Trafficking","Eating Disorder","Powerball (Lottery)","Emmanuel Macron (President of France)","Germany","F1","Colgate","Child Labor","Springer Spaniel","ESPN","Casablanca","Am I Pregnant","Deforestation","Guns","The Dress (black/blue or gold/white)","Andre The Giant","Metropolis","Six Feet Under (TV)","Usain Bolt","Brave New World","The Supremes (band)","Photoshop","Taj Mahal","Aurora","Dictionary","Louis Vuitton","Tax Avoidance","Moses","Making a Murderer (TV)","Oculus Rift","Kayak","Easter","The Green Mile (Movie)","Pac-Man","Celine Dion","Seoul (Korea)","Outlook","Womens Rights","Tiananmen Square","Tony Blair (Former British PM)","Milan","Gym","Terracotta Warriors","Boca Juniors (soccer team)","Phil Collins","The Big Lebowski (Movie)","Diana Ross","Refugee","Faith Healing","Maradona","Psoriasis","Halloween","Easter Island","IMDB","Victoria Secret","Hunger","Samaritans","Peppa Pig","Homophobia","Thesaurus","Athletics","Buzzfeed","Unilever","Taiwan","Julian Assange (founder of Wikileaks)","V For Vendetta","Calculator","The Intouchables (Movie)","ISIS","Burger King","Jupiter","Pot Noodles","Masai Mara","No Country For Old Men (Movie)","Ohio Stadium","Ferrari","Car Wash","British Airways","Interstellar","Vodafone","Nepal Earthquake","How I Met Your Mother (TV)","YSL (Fashion)","Barcelona","Liposuction","Greenpeace (Non-profit) ","20th Century Fox","Game Of Thrones","Waterskiing","Geocaching","Overwatch","Truman Show","Arsenal","Mental Health","Arthritis","Anna Kournikova (tennis player)","Augmented Reality","Teacher Training","Depression","Vantablack","Puberty","Obesity","Bank Of America","Goodfellas (Movie)","Puerto Rico","Roger Federer","China Mobile","The Little Prince (book)","Hip Hop","Edward Snowden","Vegetarian","The Marshmallow Test","Driving Instructor","Air Hockey","Minecraft","James Foley (American Journalist)","Bob Dylan","Paragliding","Smoked Salmon","Wall Street Bombing","Tai Chi","This American Life (Radio)","Machu Picchu","Monopoly","Dry Cleaners","Pensions","Coffee Beans","Senegal","Code Computerlove (Marketing)","Mothers Day","Australian Open","Rowing","Meditation","No Mans Sky","Juan Manuel Santos (President of Columbia)","Pagan","Angry Birds","Asthma","Water Pollution","Wuthering Heights (novel)","The Bourne Ultimatum","Jeep","Bradley Cooper","Mount Sinai","Hong Kong","Chelsea Manning","Wayne Rooney","Miss Universe","Handball","Bryan Adams","Mozart","Jet Li","Border Collie","London","Moon Landing","Car Rental","Erectile Dysfunction","Cool Runnings (Movie)","Wordpress","Fifty Shades of Grey","Mercedes Benz","BNP Paribas (Bank)","KitKat","Slimfast","Colosseum","Beach Volleyball","Budweiser","Palm Islands","Gambling","Snakes and Ladders","The Temptations (Band)","McDonalds","Pokemon","Pistol","Weightlifting","The Psychopath Test","Donna Summer","Taxi","Sailing","Fake News","Dark Souls III","Christmas","Volkswagen","Earth","Twitch","Royal Canin","Mario Lemieux (Hockey Player)","Baidu","Flute","Cristiano Ronaldo","Greek Salad","Bill Russell (Basketball Player)","Brian Clough (Football Player)","True Detective","Loans","Karate","Nigel Mansell (F1 Racer)","Chicago White Sox (Baseball Team)","Tightrope Walking","Tottenham Hotspur","Margaret Thatcher","Parkinsons Disease","Iran","Don Quitoxe","Boston Red Sox (Baseball Team)","Royal Robbins (Rock Climber)","Divorce Lawyer","Rome","Heineken","Rose Bowl (Football)","One Direction","Janet Jackson","Power Rangers","Sin City (Movie)","The Great Escape (Movie)","The Hitchhikers Guide To The Galaxy","Adderall","Banana Cake","Austerity","Aldi (Supermarket)","Qatar Airways","Domestic Abuse","Angkor Wat","Halong Bay","Larisa Latynina (Gymnast)","Charlie Hebdo (Cartoonist)","Serena Williams","Super Mario Run","Istanbul","Jeremy Clarkson (Broadcaster)","Bhola Cyclone","Xanax","Pest Controller","Martin Luther King","Coffee Shop","Masseuse","Coincidence","The Dark Night Rises","Clint Eastwood","Find My iPhone","Skin Cancer","Porsche","Swegways","Vandalism","Paul Pogba","Comcast","HSBC","Gillette","Blackfish","Cankles","Poverty","Apple Watch","The Usual Suspect (Movie)","Andre Agassi","Sexism","Larry Bird (Basketball Player)","Trumpet","South Africa","Adam Curtis (Film Maker)","Yoga","Hotmail","Manslaughter","French Bulldog","Uno","Andy Murray","Reservoir Dogs (Movie)","Xbox One","Bulimia","Uber","A Tale Of Two Cities (novel)","The Establishment (novel)","Donald Trump","Free Climbing","Bali Bombing","Xi Jinping (General Secretary of China)","Kareem Abdul-Jabbar","Hilton","Django Unchained","Ashley Madison","Fight Club (Movie)","Adoption","Caitlyn Jenner","Poland","Plato","Kuala Lumpur","Street Fighter","Into The Wild (novel)","Berlin Wall","Blue Lagoon"," Juventus (Soccer Team)","Mercury","Pink Floyd","Western Union","Beauty","T-Mobile","The Beatles","Marathon","Ariana Grande","Santiago Bernabeau","Lennox Lewis (Boxer)","Prison Break","Girl With A Pearl Earring (Painting)","Stroke","Pans Labyrinth","Tokyo","France","Pepsi","Raiders Of The Lost Ark","Amnesty","Malcolm X","Christ The Redeemer","Michael Schumacher (Racer)","Madonna","Stanley Kubrick (Director)","Wembley Stadium","American Beauty (Movie)"]

--political searches values
poliNumList = ["4090000","1500000","2740000","4090000","673000","100500","60500","1220000","67300","605000","301000","3350000","2740000","823000","401000","30500","166000","40500","201100","560000","910000","4090000","165000","2240000","30400000","9200000","1220000","148000","2220000","9140000","1000000","1220000","368000","823000","37200000","1000000","201100","245000","90500","550000","90000","4090000"]
poliNumList2 = [4090000,1500000,2740000,4090000,673000,100500,60500,1220000,67300,605000,301000,3350000,2740000,823000,401000,30500,166000,40500,201100,560000,910000,4090000,165000,1500000,2240000,30400000,9200000,1230000,148000,2220000,9140000,1000000,1220000,368000,823000,37200000,1000000,201100,245000,90500,550000,90000,4090000]
poliList = ["Christmas","Fake News","Moon Landing","Water Pollution","Pagan","Juan Manuel Santos (President of Columbia)","Pensions","Wall Street Bombing","James Foley (American Journalist)","Edward Snowden","Puberty","Obesity","Mental Health","Greenpeace","ISIS","Julian Assange (founder of Wikileaks)","Homophobia","Refugee","Faith Healing","Tony Blair (Former British PM)","Womens Rights","Taxes","Moses","Child Labor","Emmanuel Macron (President of France)","Chinese New Year","Jeremy Corbyn","Transgender","Anti Semitism","Human Rights","Jesus","World Hunger","Capitalism","Hinduism","Wikileaks","Putin","Prince Charles","General Election","Muslim","Quran","Easter","Human Trafficing","Illuminati"]

--media searches values
mediaNumList = ["1830000","201000","2240000","1500000","40500","673000","823000","246000","368000","201000","8100","450000","1500000","4090000","201000","11100000","9140000","135000","368000","60500","2240000","1220000","7480000","673000","20400000","90500","550000","49500","2740000","1830000","18100","90500","74000","11100000","6120000","550000","823000","6120000","11100000","672000","823000","335000","134000","450000","550000","27100","1220000","246000","1220000","8100","673000","1500000","246000","368000","90500","135000","5000000","165000","450000","18100"]
mediaNumList2 = [1830000,201000,2240000,1500000,40500,673000,823000,246000,368000,201000,8100,450000,1500000,4090000,201000,11100000,9140000,135000,368000,60500,2240000,1220000,7480000,673000,20400000,90500,550000,49500,2740000,1830000,18100,90500,74000,11100000,6120000,550000,823000,6120000,11100000,672000,823000,335000,134000,450000,550000,27100,1220000,246000,1220000,8100,673000,1500000,246000,368000,90500,135000,5000000,165000,450000,18100]
mediaList = ["Game Of Thrones","The New York Times","Adele","Theresa May","Violin","News","There Will Be Blood (Movie)","Cartoons","The Black Eye Peas","ABBA (band)","The Diary of Anne Frank","TED (talks)","Coachella Festival","Playstation VR","Six Nations","Wes Anderson","Batman","Myspace","Love Island (TV)","The Secret Garden (Book)","Britains Got Talent","Marie Curie","Michael Jordan","Mirrors Edge (Video Game)","Valentines Day","Gangnam Style","Willie Mays","Schlinders List","Person Of Interest (TV)","Family Guy","Saxophone","Leicester City","Lionel Richie","Mark Zuckerberg","The Walking Dead","Mickey Mouse","Aretha Franklin","Breaking Bad","Taylor Swift","NWA (Rap group)","Four Tops (Band)","Rod Stewart","Justin Timberlake","Madden 16","Spirit Away","Looney Tunes","Willy Wonka","Peaky Blinders","Gran Torino","The Big Bang Theory","The Catcher In The Rye","Teletubbies","George Foreman","The Pianist","George Michael","Requiem For A Dream (Movie)","Popeye","Call of Duty","Big Brother","Pokemon GO","Gladiator"]

--brands and accessories searches values
accessNumList = ["823000","1500000","3350000","2240000","3140000","673000","2740000","823000","3350000","1830000","33100","270000","450000","1220000","201000","2240000","10140000","4090000","9150000","1440000","1320000","14800","2500000","750000","1420000","1550000","550000","1600000","1220000","9140000","135000","40500","302000","368000","1500000","450000","165000","83100000","301000","1000000","1830000","226000000","1500000","246000","365000","673000","550000","37200000","3350000","90500","15000000","823000","368000"]
accessNumList2 = [823000,1500000,3350000,2240000,3140000,673000,2740000,823000,3350000,1830000,33100,270000,450000,1220000,201000,2240000,10140000,4090000,9150000,1440000,1320000,14800,2500000,750000,1420000,1550000,550000,1600000,1220000,9140000,135000,40500,302000,368000,1500000,450000,165000,83100000,301000,1000000,1830000,226000000,1500000,246000,365000,673000,550000,37200000,3350000,90500,1500000,823000,368000]
accessList= ["Baidu","Volkswagen","McDonalds","Pokemon","Budweiser","KitKat","Mercedes Benz","BNP Paribas (Bank)","Wordpress","Car Rental","Code Computerlove (Marketing)","China Mobile","Burger King","Ferrari","British Airways","Outlook","Oculus Rift","Louis Vuitton","Photoshop","Powerball (Lottery)","General Electric (GE)","Free Tibet","Audi","Verizon","Doritos","Microsoft","General Mills","Toyota","Lacoste","Paypal","Fitbit","Uncle Bens (food)","Prada","DMT","Antidepressants","Cadbury","Bovril (Yeast Drink)","Emirates (Airline)","Burberry","Converse","Pampers","Spotify","Guinness","Mini Cooper","Kraft","Boeing","VW Golf","IKEA","HP","Dyson","Samsung","American Express","Limousine"]

--sport related searches values
sportNumList = ["90500","3680000","122000","1220000","60500","246000","673000","950000","1650000","274000","1650000","40500","7480000","1000000","823000","49500","1220000","90500","9140000","2740000","90400","40600","2240000","165000","301000","40500","110000","165000","823000","450000","82300","550000","33100","245000","5000000","301000","8100","90500","1300","246000","368000","365000","1830000","673000","1220000"]
sportNumList2 = [90500,3680000,122000,1220000,60500,246000,673000,950000,1650000,274000,1650000,40500,7480000,1000000,823000,49500,1220000,90500,9140000,2740000,90400,40600,2240000,165000,301000,40500,110000,165000,823000,450000,82300,550000,33100,245000,5000000,301000,8100,90500,1300,246000,369000,368000,1830000,673000,1220000]
sportList = ["Waterskiing","Arsenal","Anna Kournikova (tennis player)","Paragliding","Australian Open","Rowing","Wayne Rooney","FA Cup","AC Milan","Seattle Mariners (Baseball Team)","Swimming","Rugby World Cup","Super Bowl","ESPN","Usain Bolt","Bolton Wanderers (Soccer team)","Boxing","Rock Climbing","Rafael Nadal","Snowboard","John McEnroe (Tennis Player)","Ice Climbing","Eden Hazard","Owen Hart","San Francisco 49ers","Hang Gliding","Flamengo (Soccer team)","Scuba Diving","Sky Diving","Wii Sports","Bjorn Borg (Tennis Player)","Franz Beckenbauer","Triple H (WWE)","Bruce Springsteen","Novak Djokovic","Chicago Bulls","Wilt Chamberlain","Fencing","Volcano Surfing","New York Jets","Tampa Bay Buccaneers","Thiery Henry","Luis Suarez","New York Mets","Los Angeles Lakers"]

--history, geography and astronomy searches values
histGeoNumList = ["11100000","135000","90500","2240000","49500","450000","201000","4550000","74000","15000000","3350000","450000","1830000","33100","1830000","823000","368000","12100","2740000","175000","110000","350000","145000","1200000","1500000","90500","9140000","303000","201000","135000","1500000","550000","450000","165000","1350000","4090000","1000000","301000","1500000","90500","245000","49500","1220000","5000000","1830000","18100"]
histGeoNumList2 = [11100000,135000,90500,2240000,49500,450000,201000,4550000,74000,15000000,3350000,450000,1830000,33100,183000,823000,368000,12100,2740000,175000,110000,350000,145000,1200000,1500000,90500,9140000,303000,201000,135000,1600000,550000,450000,165000,1350000,4090000,1000000,301000,1500000,90500,245000,49500,1220000,5000000,1830000,18100]
hitsGeoList = ["Iran","Rome","Istanbul","South Africa","Berlin Wall","Tokyo","France","Colosseum","Palm Islands","Earth","London","Mount Sinai","Hong Kong","Senegal","Machu Picchu","Puerto Rico","Barcelona","Nepal","Jupiter","Taiwan","Easter Island","Milan","Tiananmen Square","Seoul (Korea)","Taj Mahal","Casablanca","Germany","Potala Palace","Egyptian Pyramids","Blood Moon","Spain","Saudi Arabia","Holocaust","Taipei","Silicon Valley","New York","Venus","Uganda","Israel","Black Hole","Winter Solstice","Kata Tjuta","UFO","Maylaysia","Las Vegas","Stonehenge"]

difficultyThresholds = [1000000, 500000, 100000, 50000, 10000, 5000]
difficultyThresholdsSub = [1000000, 750000, 500000, 250000, 100000, 50000]

-- test for correcness
test y z a b c d = do
    if y >= z
        then goon b c d
        else wrong a b c

goon b c d = do
    putStrLn "Correct!"
    putStrLn (searchList!!b ++ " has " ++ numList!!b ++ " average monthly searches!") 
    let k = c
    let c = k+1
    putStrLn "Number of correct guesses so far: "
    print c
    --putStrLn "Difficulty level is: "
    let i = d + 1
    if i > 5 then updatedifficulty c d d
    else updatedifficulty c d i
    --print d
    --getData c d
    --playG c d

wrong a b c = do
    putStrLn "Wrong!"
    putStrLn (searchList!!a ++ " : " ++ numList!!a)
    putStrLn (searchList!!b ++ " : " ++ numList!!b)
    putStrLn "Total number of correct guesses this round is: "
    print c

updatedifficulty c d i = do
    if c `mod` 5 == 0 
    then getData c i
    else getData c d


isOne x = do
    x > 0 && x < 2

isZero x = do
    x > -1 && x < 1

-- https://github.com/joshfinnie/haskell-guessing-game/blob/master/guessing_game_2.hs
-- thanks to this link, was able understand random number generator and make the game always unique
-- and allow for us to take 1 or 0 to function the game



getData c difficulty = do
    newStdGen
    genA <- getStdGen
    newStdGen
    genB <- getStdGen  
    let (a, _) = randomR (0,547) genA :: (Int, StdGen)
    let (b, _) = randomR (0,547) genB :: (Int, StdGen)
    let val1 = numList2!!a
    let val2 = numList2!!b
    let difference = abs(val1 - val2)
    let threshold = difficultyThresholds!!difficulty
    if difference > threshold then getData c difficulty
    else playG a b c difficulty


-- runs the correct version of code based on category chosen
myFunc newStr = do
    let c = 0;
    let d = 0;
    if newStr == 6 then getData c d
    else if newStr == 1 then getDataM c d
    else if newStr == 2 then getDataP c d
    else if newStr == 3 then getDataA c d
    else if newStr == 4 then getDataS c d
    else if newStr == 5 then getDataH c d
    else if newStr == 6 then getData c d
    else putStrLn("wrong input")


main = do
    let c = 0
    putStrLn("choose a category: 1) media   2)political,cultural and religious   3)brands and accessories   4)sports   5)history, geography and astronomy   6) general")
    newStr <- getLine
    let number = read newStr;
    myFunc number
    	
-- general questions
playG a b c d = do
    putStrLn("Difficulty level: ")
    print d
    putStrLn ("Does " ++ searchList!!b ++ " have fewer (0) or more (1) average monthly searches than " ++ searchList!!a ++ " ("++ numList!!a ++ " average monthly searches)?")
    numberString <- getLine
    when (not $ null numberString) $do
        let y = numList2!!b
        let z = numList2!!a
        let number = read numberString
        if isOne number
            then test y z a b c d
        else if isZero number
            then test z y a b c d
        else putStrLn "Wrong input"


--political questions
testP y z a b c d = do
    if y >= z
        then goonP b c d
        else wrongP a b c

goonP b c d = do
    putStrLn "Correct!"
    putStrLn (poliList!!b ++ " has " ++ poliNumList!!b ++ " average monthly searches!") 
    let k = c
    let c = k+1
    putStrLn "Number of correct guesses so far: "
    print c
    let i = d + 1
    if i > 5 then updatedifficultyP c d d
    else updatedifficultyP c d i

wrongP a b c = do
    putStrLn "Wrong!"
    putStrLn (poliList!!a ++ " : " ++ poliNumList!!a)
    putStrLn (poliList!!b ++ " : " ++ poliNumList!!b)
    putStrLn "Total number of correct guesses this round is: "
    print c

updatedifficultyP c d i = do
    if c `mod` 5 == 0 
    then getDataP c i
    else getDataP c d


getDataP c difficulty = do
    newStdGen
    genA <- getStdGen
    newStdGen
    genB <- getStdGen  
    let (a, _) = randomR (0,42) genA :: (Int, StdGen)
    let (b, _) = randomR (0,42) genB :: (Int, StdGen)
    let val1 = poliNumList2!!a
    let val2 = poliNumList2!!b
    let difference = abs(val1 - val2)
    let threshold = difficultyThresholdsSub!!difficulty
    if difference > threshold then getDataP c difficulty
    else playP a b c difficulty


playP a b c d = do
    putStrLn("Difficulty level: ")
    print d
    putStrLn ("Does " ++ poliList!!b ++ " have fewer (0) or more (1) average monthly searches than " ++ poliList!!a ++ " ("++ poliNumList!!a ++ " average monthly searches)?")
    numberString <- getLine
    when (not $ null numberString) $do
        let y = poliNumList2!!b
        let z = poliNumList2!!a
        let number = read numberString
        if isOne number
            then testP y z a b c d
        else if isZero number
            then testP z y a b c d
        else putStrLn "Wrong input"

-- Media related questions
testM y z a b c d = do
    if y >= z
        then goonM b c d
        else wrongM a b c

goonM b c d = do
    putStrLn "Correct!"
    putStrLn (mediaList!!b ++ " has " ++ mediaNumList!!b ++ " average monthly searches!") 
    let k = c
    let c = k+1
    putStrLn "Number of correct guesses so far: "
    print c
    let i = d + 1
    if i > 5 then updatedifficultyM c d d
    else updatedifficultyM c d i

wrongM a b c = do
    putStrLn "Wrong!"
    putStrLn (mediaList!!a ++ " : " ++ mediaNumList!!a)
    putStrLn (mediaList!!b ++ " : " ++ mediaNumList!!b)
    putStrLn "Total number of correct guesses this round is: "
    print c


updatedifficultyM c d i = do
    if c `mod` 5 == 0 
    then getDataM c i
    else getDataM c d

getDataM c difficulty = do
    newStdGen
    genA <- getStdGen
    newStdGen
    genB <- getStdGen  
    let (a, _) = randomR (0,59) genA :: (Int, StdGen)
    let (b, _) = randomR (0,59) genB :: (Int, StdGen)
    let val1 = mediaNumList2!!a
    let val2 = mediaNumList2!!b
    let difference = abs(val1 - val2)
    let threshold = difficultyThresholdsSub!!difficulty
    if difference > threshold then getDataM c difficulty
    else playM a b c difficulty




playM a b c d = do
    putStrLn ("Does " ++ mediaList!!b ++ " have fewer (0) or more (1) average monthly searches than " ++ mediaList!!a ++ " ("++ mediaNumList!!a ++ " average monthly searches)?")
    numberString <- getLine
    when (not $ null numberString) $do
        let y = mediaNumList2!!b
        let z = mediaNumList2!!a
        let number = read numberString
        if isOne number
            then testM y z a b c d
        else if isZero number
            then testM z y a b c d
        else putStrLn "Wrong input"
-- Accessories and brands questions
testA y z a b c d = do
    if y >= z
        then goonA b c d
        else wrongA a b c
goonA b c d = do
    putStrLn "Correct!"
    putStrLn (accessList!!b ++ " has " ++ accessNumList!!b ++ " average monthly searches!") 
    let k = c
    let c = k+1
    putStrLn "Number of correct guesses so far: "
    print c
    let i = d + 1
    if i > 5 then updatedifficultyA c d d
    else updatedifficultyA c d i

wrongA a b c = do
    putStrLn "Wrong!"
    putStrLn (accessList!!a ++ " : " ++ accessNumList!!a)
    putStrLn (accessList!!b ++ " : " ++ accessNumList!!b)
    putStrLn "Total number of correct guesses this round is: "
    print c

updatedifficultyA c d i = do
    if c `mod` 5 == 0 
    then getDataA c i
    else getDataA c d


getDataA c difficulty = do
    newStdGen
    genA <- getStdGen
    newStdGen
    genB <- getStdGen  
    let (a, _) = randomR (0,52) genA :: (Int, StdGen)
    let (b, _) = randomR (0,52) genB :: (Int, StdGen)
    let val1 = accessNumList2!!a
    let val2 = accessNumList2!!b
    let difference = abs(val1 - val2)
    let threshold = difficultyThresholdsSub!!difficulty
    if difference > threshold then getDataA c difficulty
    else playA a b c difficulty

playA a b c d = do
    putStrLn("Difficulty level: ")
    print d
    putStrLn ("Does " ++ accessList!!b ++ " have fewer (0) or more (1) average monthly searches than " ++ accessList!!a ++ " ("++ accessNumList!!a ++ " average monthly searches)?")
    numberString <- getLine
    when (not $ null numberString) $do
        let y = accessNumList2!!b
        let z = accessNumList2!!a
        let number = read numberString
        if isOne number
            then testA y z a b c d
        else if isZero number
            then testA z y a b c d
        else putStrLn "Wrong input"
-- sport related questions
testS y z a b c d = do
    if y >= z
        then goonS b c d
        else wrongS a b c
goonS b c d = do
    putStrLn "Correct!"
    putStrLn (sportList!!b ++ " has " ++ sportNumList!!b ++ " average monthly searches!") 
    let k = c
    let c = k+1
    putStrLn "Number of correct guesses so far: "
    print c
    let i = d + 1
    if i > 5 then updatedifficultyS c d d
    else updatedifficultyS c d i

wrongS a b c = do
    putStrLn "Wrong!"
    putStrLn (sportList!!a ++ " : " ++ sportNumList!!a)
    putStrLn (sportList!!b ++ " : " ++ sportNumList!!b)
    putStrLn "Total number of correct guesses this round is: "
    print c


updatedifficultyS c d i = do
    if c `mod` 5 == 0 
    then getDataS c i
    else getDataS c d

getDataS c difficulty = do
    newStdGen
    genA <- getStdGen
    newStdGen
    genB <- getStdGen  
    let (a, _) = randomR (0,44) genA :: (Int, StdGen)
    let (b, _) = randomR (0,44) genB :: (Int, StdGen)
    let val1 = sportNumList2!!a
    let val2 = sportNumList2!!b
    let difference = abs(val1 - val2)
    let threshold = difficultyThresholdsSub!!difficulty
    if difference > threshold then getDataS c difficulty
    else playS a b c difficulty


playS a b c d = do
    putStrLn("Difficulty level: ")
    print d
    putStrLn ("Does " ++ sportList!!b ++ " have fewer (0) or more (1) average monthly searches than " ++ sportList!!a ++ " ("++ sportNumList!!a ++ " average monthly searches)?")
    numberString <- getLine
    when (not $ null numberString) $do
        let y = sportNumList2!!b
        let z = sportNumList2!!a
        let number = read numberString
        if isOne number
            then testS y z a b c d
        else if isZero number
            then testS z y a b c d
        else putStrLn "Wrong input"
-- History, geography and astronomy related questions
testH y z a b c d = do
    if y >= z
        then goonH b c d
        else wrongH a b c
goonH b c d = do
    putStrLn "Correct!"
    putStrLn (hitsGeoList!!b ++ " has " ++ histGeoNumList!!b ++ " average monthly searches!") 
    let k = c
    let c = k+1
    putStrLn "Number of correct guesses so far: "
    print c
    let i = d + 1
    if i > 5 then updatedifficultyH c d d
    else updatedifficultyH c d i

wrongH a b c = do
    putStrLn "Wrong!"
    putStrLn (hitsGeoList!!a ++ " : " ++ histGeoNumList!!a)
    putStrLn (hitsGeoList!!b ++ " : " ++ histGeoNumList!!b)
    putStrLn "Total number of correct guesses this round is: "
    print c

updatedifficultyH c d i = do
    if c `mod` 5 == 0 
    then getDataH c i
    else getDataH c d

getDataH c difficulty = do
    newStdGen
    genA <- getStdGen
    newStdGen
    genB <- getStdGen  
    let (a, _) = randomR (0,45) genA :: (Int, StdGen)
    let (b, _) = randomR (0,45) genB :: (Int, StdGen)
    let val1 = histGeoNumList2!!a
    let val2 = histGeoNumList2!!b
    let difference = abs(val1 - val2)
    let threshold = difficultyThresholdsSub!!difficulty
    if difference > threshold then getDataH c difficulty
    else playH a b c difficulty

playH a b c d = do
    putStrLn("Difficulty level: ")
    print d
    putStrLn ("Does " ++ hitsGeoList!!b ++ " have fewer (0) or more (1) average monthly searches than " ++ hitsGeoList!!a ++ " ("++ histGeoNumList!!a ++ " average monthly searches)?")
    numberString <- getLine
    when (not $ null numberString) $do
        let y = histGeoNumList2!!b
        let z = histGeoNumList2!!a
        let number = read numberString
        if isOne number
            then testH y z a b c d
        else if isZero number
            then testH z y a b c d
        else putStrLn "Wrong input"


