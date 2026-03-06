#include "Registry.h"
#include "DPIAware.h"
#include "Event.h"
#include "TEAWin32.h"

#include <stdio.h>
#include <windows.h>

#define UNIQUEID_HWND_MAP_PAGE_MAX 1024
#define UNIQUEID_HWND_MAP_PAGE_SIZE 1024

typedef struct
{
    int uniqueId;
    HWND correspondingHWND;
} UniqueIdHWNDMapEntry;

static UniqueIdHWNDMapEntry *UNIQUEID_HWND_MAP_PAGE_TABLE_POSITIVE_ID[UNIQUEID_HWND_MAP_PAGE_SIZE] = {NULL};
static UniqueIdHWNDMapEntry *UNIQUEID_HWND_MAP_PAGE_TABLE_NEGATIVE_ID[UNIQUEID_HWND_MAP_PAGE_SIZE] = {NULL};

static inline BOOL CalculatePageIdxAndOffset(
    int uniqueId, int *pageIdxPtr, int *offsetPtr, UniqueIdHWNDMapEntry ***pageTablePtr)
{
    if (uniqueId == 0)
    {
        NotifyFatalError(L"UniqueId 0 is given.", L"CalculatePageIdxAndOffset (Registry.c)");
        return FALSE;
    }

    unsigned int absoluteUniqueId = (uniqueId > 0) ? (unsigned int)uniqueId : (unsigned int)(-uniqueId);
    int pageIdx = absoluteUniqueId / UNIQUEID_HWND_MAP_PAGE_SIZE;
    int offset = absoluteUniqueId % UNIQUEID_HWND_MAP_PAGE_SIZE;

    if (pageIdx >= UNIQUEID_HWND_MAP_PAGE_MAX)
    {
        NotifyFatalError(L"UNIQUEID_HWND_MAP_PAGE_TABLE Overflow.", L"CalculatePageIdxAndOffset (Registry.c)");

        return FALSE;
    }

    *pageIdxPtr = pageIdx;
    *offsetPtr = offset;

    if (uniqueId > 0)
    {
        *pageTablePtr = UNIQUEID_HWND_MAP_PAGE_TABLE_POSITIVE_ID;
    }
    else
    {
        *pageTablePtr = UNIQUEID_HWND_MAP_PAGE_TABLE_NEGATIVE_ID;
    }

    return TRUE;
}

HWND GetHWNDFromUniqueId(int uniqueId)
{
    int pageIdx;
    int offset;
    UniqueIdHWNDMapEntry **pageTable;

    if (!CalculatePageIdxAndOffset(uniqueId, &pageIdx, &offset, &pageTable))
    {
        return NULL;
    }

    UniqueIdHWNDMapEntry *page = pageTable[pageIdx];

    if (page == NULL)
    {
        NotifyFatalError(L"Page was NULL.", L"GetHWNDFromUniqueId (Registry.c)");

        return NULL;
    }

    return page[offset].correspondingHWND;
}

void RegisterHWNDToRegistry(HWND hwnd, int uniqueId)
{
    DEBUG_LOG(L"Registering HWND %p (UniqueId %d) to Registry.", (void *)hwnd, uniqueId);

    int pageIdx;
    int offset;
    UniqueIdHWNDMapEntry **pageTable;

    if (!CalculatePageIdxAndOffset(uniqueId, &pageIdx, &offset, &pageTable))
    {
        return;
    }

    if (pageTable[pageIdx] == NULL)
    {
        pageTable[pageIdx] = (UniqueIdHWNDMapEntry *)calloc(UNIQUEID_HWND_MAP_PAGE_SIZE, sizeof(UniqueIdHWNDMapEntry));

        if (pageTable[pageIdx] == NULL)
        {
            NotifyFatalError(L"calloc Failed", L"RegisterHWNDToRegistry (Registry.c)");
            return;
        }
    }

    pageTable[pageIdx][offset].uniqueId = uniqueId;
    pageTable[pageIdx][offset].correspondingHWND = hwnd;

    HWNDRegistryEntry *regEntry = malloc(sizeof(HWNDRegistryEntry));
    ZeroMemory(regEntry, sizeof(HWNDRegistryEntry));
    regEntry->uniqueId = uniqueId;
    regEntry->dpi = GetDPI(hwnd);

    SetWindowLongPtrW(hwnd, GWLP_USERDATA, (LONG_PTR)regEntry);

    DEBUG_LOG(L"Registered HWND %p (UniqueId %d) to Registry.", (void *)hwnd, uniqueId);
}

HWNDRegistryEntry *GetHWNDRegistryEntry(HWND hwnd)
{
    return (HWNDRegistryEntry *)GetWindowLongPtrW(hwnd, GWLP_USERDATA);
}

void UnregisterHWNDFromRegistry(HWND hwnd)
{
    DEBUG_LOG(L"Unregistring HWND %p from Registry.", (void *)hwnd);

    HWNDRegistryEntry *regEntry = GetHWNDRegistryEntry(hwnd);
    if (regEntry == NULL)
    {
        NotifyFatalError(L"HWNDRegistry entry was NULL.", L"UnregisterHWNDFromRegistry (Registry.c)");
        return;
    }

    int uniqueId = regEntry->uniqueId;

    int pageIdx;
    int offset;
    UniqueIdHWNDMapEntry **pageTable;
    if (CalculatePageIdxAndOffset(uniqueId, &pageIdx, &offset, &pageTable))
    {
        if (pageTable[pageIdx] != NULL)
        {
            pageTable[pageIdx][offset].uniqueId = 0;
            pageTable[pageIdx][offset].correspondingHWND = NULL;
        }
    }

    free(regEntry);
    SetWindowLongPtrW(hwnd, GWLP_USERDATA, (LONG_PTR)NULL);
}
