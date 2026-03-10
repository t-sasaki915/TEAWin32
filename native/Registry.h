#ifndef REGISTRY_H
#define REGISTRY_H

#include <windows.h>

typedef struct
{
    int uniqueId;
    int dpi;
    BOOL hasBackgroundColour;
    COLORREF backgroundColour;
} HWNDRegistryEntry;

BOOL GetHWNDFromUniqueId(int uniqueId, HWND *resultPtr);

BOOL RegisterHWNDToRegistry(HWND hwnd, int uniqueId);

BOOL GetHWNDRegistryEntry(HWND hwnd, HWNDRegistryEntry **resultPtr);

BOOL UnregisterHWNDFromRegistry(HWND hwnd);

#endif
