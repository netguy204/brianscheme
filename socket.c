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

#include "types.h"
#include "gc.h"
#include "interp.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

DEFUN1(server_socket_proc) {
  struct sockaddr_in addr;
  long port = LONG(FIRST);
  int sock;

  sock = socket(AF_INET, SOCK_STREAM, 0);
  if(sock < 0) {
    return g->false;
  }

  memset(&addr, 0, sizeof(struct sockaddr_in));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port = htons(port);

  if(bind(sock, (struct sockaddr*)&addr, sizeof(struct sockaddr_in)) < 0) {
    return g->false;
  }

  if(listen(sock, 10) < 0) {
    return g->false;
  }

  return make_fixnum(sock);
}

DEFUN1(socket_accept_proc) {
  long socket = LONG(FIRST);
  int conn_socket;

  conn_socket = accept(socket, NULL, NULL);
  if(conn_socket < 0) {
    return g->false;
  }

  return make_fixnum(conn_socket);
}

DEFUN1(socket_read_proc) {
  long socket = LONG(FIRST);
  long n_bytes = LONG(SECOND);

  object * bytes = make_filled_string(n_bytes + 1, '\0');
  push_root(&bytes);
  long rb = read(socket, STRING(bytes), n_bytes);

  object * result = g->empty_list;
  push_root(&result);
  object * count = make_fixnum(rb);
  push_root(&count);

  result = cons(bytes, result);
  result = cons(count, result);

  pop_root(&count);
  pop_root(&result);
  pop_root(&bytes);

  return result;
}

DEFUN1(socket_write_proc) {
  long socket = LONG(FIRST);
  char * data = STRING(SECOND);
  long nbytes = LONG(THIRD);

  long written = write(socket, data, nbytes);
  return make_fixnum(written);
}

DEFUN1(socket_close_proc) {
  long socket = LONG(FIRST);
  if(close(socket) < 0) {
    return g->false;
  }
  return g->true;
}

void init_socket(definer defn) {
  defn("make-server-socket",
       make_primitive_proc(server_socket_proc));
  defn("socket-accept",
       make_primitive_proc(socket_accept_proc));
  defn("socket-read",
       make_primitive_proc(socket_read_proc));
  defn("socket-write",
       make_primitive_proc(socket_write_proc));
  defn("socket-close",
       make_primitive_proc(socket_close_proc));
}
