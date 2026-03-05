#ifndef EVENT_H
#define EVENT_H

#include <windows.h>

typedef enum
{
    EVENT_TYPE_TEST_EVENT = 0,
    EVENT_TYPE_FATAL_ERROR
} EventType;

typedef struct
{
    LPCWSTR errorType;
    DWORD errorCode;
    LPCWSTR errorLocation;
} FatalErrorEventData;

typedef struct
{
    EventType eventType;
    union {
        FatalErrorEventData fatalErrorEventData;
    } eventData;
} EventQueueEntry;

typedef void (*PEVENTENQUEUER)(const EventQueueEntry *);

void InitialiseEvent(PEVENTENQUEUER eventEnqueuer);

void QueueEvent(EventQueueEntry *newEvent);

void NotifyFatalError(LPCWSTR errorType, LPCWSTR errorLocation);

#endif
