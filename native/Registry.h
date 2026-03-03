#ifndef REGISTRY_H
#define REGISTRY_H

#include <windows.h>

typedef struct
{
    int uniqueId;
    int dpi;
} HWNDRegistryEntry;

HWND GetHWNDFromUniqueId(int uniqueId);

void RegisterHWNDToRegistry(HWND hwnd, int uniqueId);

HWNDRegistryEntry *GetHWNDRegistryEntry(HWND hwnd);

void UnregisterHWNDFromRegistry(HWND hwnd);

#endif
