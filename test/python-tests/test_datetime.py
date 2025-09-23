# Test date and time operations
from datetime import datetime, date, time, timedelta
import time as time_module

# Current date and time
now = datetime.now()
print("Current datetime:", now)
print("Current date:", now.date())
print("Current time:", now.time())

# Formatting dates
formatted = now.strftime("%Y-%m-%d %H:%M:%S")
print("Formatted datetime:", formatted)

# Parsing dates
date_string = "2023-12-25 15:30:00"
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")
print("Parsed datetime:", parsed_date)

# Date arithmetic
today = date.today()
tomorrow = today + timedelta(days=1)
yesterday = today - timedelta(days=1)
next_week = today + timedelta(weeks=1)

print("\nDate arithmetic:")
print("Today:", today)
print("Tomorrow:", tomorrow)
print("Yesterday:", yesterday)
print("Next week:", next_week)

# Time difference
date1 = date(2023, 1, 1)
date2 = date(2023, 12, 31)
difference = date2 - date1
print(f"\nDays between {date1} and {date2}: {difference.days}")

# Creating specific datetime
specific_datetime = datetime(2023, 12, 25, 15, 30, 0)
print("Specific datetime:", specific_datetime)

# Time operations
current_time = time_module.time()
print(f"Unix timestamp: {current_time}")

# Sleep demonstration
print("\nSleeping for 0.5 seconds...")
time_module.sleep(0.5)
print("Done sleeping!")

# Date components
print("\nDate components:")
print("Year:", now.year)
print("Month:", now.month)
print("Day:", now.day)
print("Hour:", now.hour)
print("Minute:", now.minute)
print("Second:", now.second)

# Weekday
weekday_names = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
print(f"Today is {weekday_names[now.weekday()]}")

# Time zone naive operations (no timezone handling)
print("\nTime operations:")
noon = time(12, 0, 0)
print("Noon time:", noon)
print("Time hour:", noon.hour)
print("Time minute:", noon.minute)