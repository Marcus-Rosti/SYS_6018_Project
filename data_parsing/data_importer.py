import json

"""
    What I want for this file is import the dataset so that it subsets on an area

    Read      Parse out         Read      Parse out
    ------    --------------    ------    -------------------
    -Busi- -> -Business IDs- -> -Revi- -> -Subset of Reviews-
    ------    --------------    ------    -------------------

    so data importer should return in the end a set of reviews all related to a single city
    This shoul narrow down the scope of the project enough to be managible.
"""

def read_businesses(file,location):
    business_ids = []
    with open(file) as f:
        for line in f:
            # if line.location == location
                # business_ids.append(line.business_id)
    return(business_ids)

def read_reviews(review_file,business_file,location):
    """

    :param review_file:     Review file location
    :param business_file:   Business File Location
    :param location:        Location we care about
    :return:                Array of lines
    """
    # read_businesses
    data = []
    business_ids = read_businesses(business_file,location)

    with open(file) as f:
        for line in f:
            if check_business_location(line,business_ids):
                data.append(json.loads(line))
    return(data)


def check_business_location(line,business_ids):
    """

    :param line: the line to check
    :param business_ids:  an array of business_ids
    :return: true
    """
    # if business_ids.contains(line.business_id)
        # return true
    # else
    return False #True or false


def main():


    print("Hello World")

if __name__ == '__main__':
    main()