/**
 * This code was originally written by Christopher Wellons
 * and was copied from his lisp implementation "wisp" which
 * can be found here:
 * http://git.nullprogram.com/?p=wisp.git;a=summary
 */

/* hashtab.c - simple hashtable */

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "hashtab.h"
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

void *xmalloc(size_t size) {
  void *p = malloc(size);
  if(p == NULL) {
    fprintf(stderr, "error: fatal: out of memory: %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }
  return p;
}

void xfree(void *p) {
  free(p);
}

hashtab_t *htb_init(size_t size, int (*hash_func) (void *, size_t)) {
  hashtab_t *new_ht = (hashtab_t *) xmalloc(sizeof(hashtab_t));
  new_ht->arr = (hashtab_node_t **) xmalloc(sizeof(hashtab_node_t *) * size);
  new_ht->size = size;
  new_ht->count = 0;

  /* all entries are empty */
  int i = 0;
  for(i = 0; i < (int)size; i++)
    new_ht->arr[i] = NULL;

  if(hash_func == NULL)
    new_ht->hash_func = &htb_hash;
  else
    new_ht->hash_func = hash_func;

  return new_ht;
}

void *htb_search(hashtab_t * hashtable, void *key) {
  int index = htb_hash(key, hashtable->size);
  if(hashtable->arr[index] == NULL)
    return NULL;

  hashtab_node_t *last_node = hashtable->arr[index];
  while(last_node != NULL) {
    if(key == last_node->key)
      return last_node->value;
    last_node = last_node->next;
  }
  return NULL;
}

void *htb_insert(hashtab_t * hashtable, void *key, void *value) {
  int index = htb_hash(key, hashtable->size);

  hashtab_node_t *next_node, *last_node;
  next_node = hashtable->arr[index];
  last_node = NULL;

  /* Search for an existing key. */
  while(next_node != NULL) {
    if(key == next_node->key) {
      next_node->value = value;
      return next_node->value;
    }
    last_node = next_node;
    next_node = next_node->next;
  }

  /* create a new node */
  hashtab_node_t *new_node;
  new_node = (hashtab_node_t *) xmalloc(sizeof(hashtab_node_t));
  new_node->key = key;
  new_node->value = value;
  new_node->next = NULL;

  /* Tack the new node on the end or right on the table. */
  if(last_node != NULL)
    last_node->next = new_node;
  else
    hashtable->arr[index] = new_node;

  hashtable->count++;
  return new_node->value;
}

/* delete the given key from the hashtable */
void htb_remove(hashtab_t * hashtable, void *key) {
  hashtab_node_t *last_node, *next_node;
  int index = htb_hash(key, hashtable->size);
  next_node = hashtable->arr[index];
  last_node = NULL;

  while(next_node != NULL) {
    if(key == next_node->key) {
      /* adjust the list pointers */
      if(last_node != NULL)
	last_node->next = next_node->next;
      else
	hashtable->arr[index] = next_node->next;

      /* free the node */
      xfree(next_node);
      break;
    }
    last_node = next_node;
    next_node = next_node->next;
  }
}

/* grow the hashtable */
hashtab_t *htb_grow(hashtab_t * old_ht, size_t new_size) {
  /* create new hashtable */
  hashtab_t *new_ht = htb_init(new_size, old_ht->hash_func);
  if(new_ht == NULL)
    return NULL;

  /* Iterate through the old hashtable. */
  hashtab_iter_t ii;
  htb_iter_init(old_ht, &ii);
  for(; ii.key != NULL; htb_iter_inc(&ii))
    htb_insert(new_ht, ii.key, ii.value);

  /* Destroy the old hashtable. */
  htb_destroy(old_ht);

  return new_ht;
}

/* free all resources used by the hashtable */
void htb_destroy(hashtab_t * hashtable) {
  hashtab_node_t *next_node, *last_node;

  /* Free each linked list in hashtable. */
  int i;
  for(i = 0; i < (int)hashtable->size; i++) {
    next_node = hashtable->arr[i];
    while(next_node != NULL) {
      /* destroy node */
      last_node = next_node;
      next_node = next_node->next;
      xfree(last_node);
    }
  }

  xfree(hashtable->arr);
  xfree(hashtable);
}

/* iterator initilaize */
void htb_iter_init(hashtab_t * hashtable, hashtab_iter_t * ii) {
  /* stick in initial bookeeping data */
  ii->internal.hashtable = hashtable;
  ii->internal.node = NULL;
  ii->internal.index = -1;

  /* have iterator point to first element */
  htb_iter_inc(ii);
}

/* iterator increment  */
void htb_iter_inc(hashtab_iter_t * ii) {
  hashtab_t *hashtable = ii->internal.hashtable;
  int index = ii->internal.index;

  /* attempt to grab the next node */
  if(ii->internal.node == NULL || ii->internal.node->next == NULL)
    index++;
  else {
    /* next node in the list */
    ii->internal.node = ii->internal.node->next;
    ii->key = ii->internal.node->key;
    ii->value = ii->internal.node->value;
    return;
  }

  /* find next node */
  while(hashtable->arr[index] == NULL && index < (int)hashtable->size)
    index++;

  if(index >= (int)hashtable->size) {
    /* end of hashtable */
    ii->internal.node = NULL;
    ii->internal.index = (int)hashtable->size;

    ii->key = NULL;
    ii->value = NULL;
    return;
  }

  /* point to the next item in the hashtable */
  ii->internal.node = hashtable->arr[index];
  ii->internal.index = index;
  ii->key = ii->internal.node->key;
  ii->value = ii->internal.node->value;
}

int htb_hash(void *key, size_t hashtab_size) {
  /* the compiler should eliminate this branch */
  uint32_t hash;

  if(sizeof(key) == 4) {
    hash = ((long)key) >> 2;
  } else {
    hash = ((long)key) >> 3;
  }

  return (hash % hashtab_size);
}
