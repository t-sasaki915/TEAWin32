#ifndef EVENT_H
#define EVENT_H

#include <windows.h>

typedef enum
{
    EVENT_TYPE_INITIAL_RENDER = 0,
    EVENT_TYPE_STOP_MAINLOOP
} EventType;

typedef struct
{
    EventType eventType;
} EventQueueEntry;

typedef void (*PEVENTENQUEUER)(const EventQueueEntry *, int);

void InitialiseEvent(PEVENTENQUEUER eventEnqueuer);

void QueueEvents(EventQueueEntry *newEvents, int eventCount);

#endif
