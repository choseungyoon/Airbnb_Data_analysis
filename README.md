# Airbnb_Data_analysis
: Where will a new guest book their first travel experience?

**Purpose**
> Predict which country a new user’s first booking destination will be. All the users in this dataset are from the USA

Possible destination country : US,FR,CA,GB,ES,IT,PT,NL,DE,AU,NDF(No destination found), other (please note that ’NDF’ is different from ‘other’ because ‘other’ means there was a booking, but is to a country not included in the list, while ‘NDF’ means there wasn’t a booking


Variable:

id : user_id

date_account_created : the date of account creation

timestamp_first_active : timestamp of the first activity, note that it can be earlier than date_account_created or date_first_booking because a user can search before signing up

date_first_booking 		: date of first booking

gender

age

signup_method

signup_flow 			: the page a user came to signup up from

language 				: international language preference

affiliate_channel		: what kind of paid marketing

affiliate_provider		: where the marketing is e.g google, craigslist, other

first_affiliate_tracked	: whats the first marketing the user interacted with before the signing up

signip_app

first_device_type

first_browser

country_destination		: the is the target variable you are to predict
