#ifndef REGISTRY_H
#define REGISTRY_H

#include <windows.h>

typedef enum
{
    COMPONENT_TYPE_WINDOW = 0,
    COMPONENT_TYPE_BUTTON
} ComponentType;

typedef struct
{
    int uniqueId;
    int dpi;
    ComponentType componentType;
} HWNDRegistryEntry;

HWND GetHWNDFromUniqueId(int uniqueId);

void RegisterHWNDToRegistry(HWND hwnd, int uniqueId, ComponentType compType);

HWNDRegistryEntry *GetHWNDRegistryEntry(HWND hwnd);

void UnregisterHWNDFromRegistry(HWND hwnd);

#endif
