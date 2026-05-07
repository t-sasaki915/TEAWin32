#include "Event.h"
#include "TEAWin32.h"

#include <stdio.h> // IWYU pragma: keep
#include <windows.h>

static PEVENTENQUEUER g_eventEnqueuerPtr;

void InitialiseEvent(PEVENTENQUEUER eventEnqueuerPtr)
{
    g_eventEnqueuerPtr = eventEnqueuerPtr;

    DEBUG_LOG(L"Event Enqueuer initialised.");
}

void QueueEvents(EventQueueEntry *newEvents, int eventCount)
{
    g_eventEnqueuerPtr(newEvents, eventCount);

#ifdef TEAWIN32_DEBUG_MODE
    for (int i = 0; i < eventCount; i++)
    {
        DEBUG_LOG(L"Event Queued. Event Type: %d", newEvents[i].eventType);
    }
#endif
}
