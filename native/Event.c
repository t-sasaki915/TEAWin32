#include "Event.h"

#include <stdio.h>
#include <windows.h>

static PEVENTENQUEUER EVENT_ENQUEUER_PTR;

void InitialiseEvent(PEVENTENQUEUER eventEnqueuerPtr)
{
    EVENT_ENQUEUER_PTR = eventEnqueuerPtr;
}

void QueueEvent(EventQueueEntry *newEvent)
{
    EVENT_ENQUEUER_PTR(newEvent);
}

void NotifyFatalError(LPCWSTR errorType, LPCWSTR errorLocation)
{
    DWORD lastErrorCode = GetLastError();

    printf("Internal C Error\r\n");
    printf("%ls: %lu\r\nLocation: %ls\r\n", errorType, lastErrorCode, errorLocation);

    EventQueueEntry event;
    ZeroMemory(&event, sizeof(event));
    event.eventType = EVENT_TYPE_FATAL_ERROR;
    event.eventData.fatalErrorEventData.errorType = errorType;
    event.eventData.fatalErrorEventData.errorCode = lastErrorCode;
    event.eventData.fatalErrorEventData.errorLocation = errorLocation;
    QueueEvent(&event);
}
