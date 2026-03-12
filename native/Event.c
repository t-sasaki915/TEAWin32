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

void QueueEvents(EventQueueEntry *newEvents, int eventCount)
{
    EVENT_ENQUEUER_PTR(newEvents, eventCount);

#ifdef TEAWIN32_DEBUG_MODE
    for (int i = 0; i < eventCount; i++)
    {
        DEBUG_LOG(L"Event Queued. Event Type: %d", newEvents[i].eventType);
    }
#endif
}
