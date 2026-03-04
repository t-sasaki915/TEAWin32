#include "Event.h"

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
