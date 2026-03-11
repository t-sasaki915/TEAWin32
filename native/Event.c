#include "Event.h"
#include "TEAWin32.h"

#include <stdio.h> // IWYU pragma: keep
#include <windows.h>

static PEVENTENQUEUER EVENT_ENQUEUER_PTR;

void InitialiseEvent(PEVENTENQUEUER eventEnqueuerPtr)
{
    EVENT_ENQUEUER_PTR = eventEnqueuerPtr;

    DEBUG_LOG(L"Event Enqueuer initialised.");
}

void QueueEvent(EventQueueEntry *newEvent)
{
    EVENT_ENQUEUER_PTR(newEvent);

    DEBUG_LOG(L"Event Queued. Event Type: %d", newEvent->eventType);
}
