/**
 * Copyright 2010 Brian Taylor
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef READ_H
#define READ_H

struct stream_reader_;

typedef struct stream_reader_ {
  int (*reader) (struct stream_reader_ *);
  void (*unreader) (struct stream_reader_ *, int);
  int (*peeker) (struct stream_reader_ *);
  void (*releaser) (struct stream_reader_ *);
} stream_reader;


int read_stream(stream_reader * stream);
void unread_stream(stream_reader * stream, int last_char);
int peek_stream(stream_reader * stream);
void release_stream(stream_reader * stream);


typedef struct file_stream_reader_ {
  stream_reader reader;
  FILE *source;
} file_stream_reader;

stream_reader *make_file_reader(FILE * file);

typedef struct string_stream_reader_ {
  stream_reader reader;
  char *source;
  int position;
} string_stream_reader;

stream_reader *make_string_reader(const char *string);

struct stream_writer_;

typedef struct stream_writer_ {
  void (*writer) (struct stream_writer_ *, char);
} stream_writer;

void write_stream(stream_writer * writer, char datum);
void stream_fprintf(stream_writer * writer, char *msg, ...);

typedef struct file_stream_writer_ {
  stream_writer writer;
  FILE *destination;
} file_stream_writer;

stream_writer *make_file_writer(FILE * file);

typedef struct string_stream_writer_ {
  stream_writer writer;
  char *destination;
  int destination_capacity;
  int position;
} string_stream_writer;

stream_writer *make_string_writer(char *buffer, int length);

struct object;

struct object *obj_read(stream_reader * in);
struct object *string_to_number(char *str);

extern char *prompt;


#endif
