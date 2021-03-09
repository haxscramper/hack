#!/usr/bin/env python2
from __future__ import print_function

import json
from pprint import pprint

import datetime
import time
import pickle
import os.path
from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request

# If modifying these scopes, delete the file token.pickle.
SCOPES = ['https://www.googleapis.com/auth/calendar']


def main():
    """Shows basic usage of the Google Calendar API.
    Prints the start and name of the next 10 events on the user's calendar.
    """
    creds = None
    # The file token.pickle stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first
    # time.
    if os.path.exists('token.pickle'):
        with open('token.pickle', 'rb') as token:
            creds = pickle.load(token)
    # If there are no (valid) credentials available, let the user log in.
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                'credentials.json', SCOPES)
            creds = flow.run_local_server(port=0)
        # Save the credentials for the next run
        with open('token.pickle', 'wb') as token:
            pickle.dump(creds, token)

    service = build('calendar', 'v3', credentials=creds)

    # Call the Calendar API
    print('Getting the upcoming 300 events')
    calId = '55p795nhpv2k54mmia2erkjhis@group.calendar.google.com'
    events_result = service.events().list(calendarId=calId,
                                          timeMin="2020-09-01T00:00:00Z",
                                          maxResults=300,
                                          singleEvents=True,
                                          orderBy='startTime').execute()
    events = events_result.get('items', [])

    if not events:
        print('No upcoming events found.')
    for event in events:
        service.events().delete(calendarId=calId,
                                eventId=event["id"]).execute()

        pprint(event)
        print("deleted")
        time.sleep(0.1)

    parsed = json.load(open("res.json"))
    for ev in parsed:
        pprint(ev)
        event = {
            'summary': ev["name"],
            'location': ev["room"],
            'description': ev["name"],
            'start': {
                'dateTime': ev["begin"],
            },
            'end': {
                'dateTime': ev["end"],
            },
            "colorId": ev["color"]
        }

        event = service.events().insert(calendarId=calId,
                                        body=event).execute()

        time.sleep(0.1)


if __name__ == '__main__':
    main()
