#ifndef EVENT_H
#define EVENT_H

typedef enum
{
    EVENT_TYPE_TEST_EVENT = 0
} EventType;

typedef struct
{
    EventType eventType;
} EventQueueEntry;

typedef void (*PEVENTENQUEUER)(const EventQueueEntry *);

void InitialiseEvent(PEVENTENQUEUER eventEnqueuer);

void QueueEvent(EventQueueEntry *newEvent);

#endif
