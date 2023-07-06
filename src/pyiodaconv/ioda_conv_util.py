# This function takes two integers like date=20180415 time=61532 and converts them to a
# ISO 8601 standard date/time string like "2018-04-15T06:15:32Z"


def IntDateTimeToString(date, time):
    # Make sure passed values are int's since we're counting on integer division
    date = int(date)
    time = int(time)
    # Define consts to prevent new int objects being created every time we use these numbers
    TEN_THOW = 10000
    HUNDRED = 100

    year = date // TEN_THOW
    date = date - year * TEN_THOW
    month = date // HUNDRED
    day = date - month * HUNDRED

    hour = time // TEN_THOW
    time = time - hour * TEN_THOW
    minute = time // HUNDRED
    second = time - minute * HUNDRED

    return "%d-%02d-%02dT%02d:%02d:%02dZ" % (year, month, day, hour, minute, second)
