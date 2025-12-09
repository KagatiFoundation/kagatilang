#ifndef OBJECT_H
#define OBJECT_H

#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum ObjectType {
    K_INT = 0,
    K_STR = 1,
    K_REC = 2
} ObjectType;

typedef struct _Object {
    uint64_t            marked;
    uint64_t            ob_size;
    uint64_t            num_children;
    struct _Object**    children;
    struct _Object*     next;
    uint64_t            ob_type;
    uint8_t*            data;
} Object;

Object* object_new(size_t size, ObjectType type, void* src);

void assign_ref(Object*, Object*, uint64_t);

#ifdef __cplusplus
}
#endif
#endif
