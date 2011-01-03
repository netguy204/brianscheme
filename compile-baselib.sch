; Copyright 2010 Brian Taylor
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
;

; DESCRIPTION: Using compiler, replace most of the methods defined in
; stdlib with compiled versions that can execute on the virtual
; machine.
;

(require 'compiler)

(replace-with-compiled cadr)
(replace-with-compiled caddr)
(replace-with-compiled cadddr)
(replace-with-compiled caddddr)

(replace-with-compiled first)
(replace-with-compiled rest)
(replace-with-compiled second)
(replace-with-compiled third)
(replace-with-compiled fourth)
(replace-with-compiled fifth)

(replace-with-compiled any?)
(replace-with-compiled every?)
(replace-with-compiled member?)
(replace-with-compiled index-of)
(replace-with-compiled list-ref)
(replace-with-compiled index-eq)

(replace-with-compiled map)
(replace-with-compiled mapr)
(replace-with-compiled length)
(replace-with-compiled reverse)
(replace-with-compiled find)
(replace-with-compiled reduce)
(replace-with-compiled append)
(replace-with-compiled mappend)

(replace-with-compiled comp-funcall)
(replace-with-compiled comp-if)
(replace-with-compiled comp-lambda)
(replace-with-compiled comp-begin)
(replace-with-compiled comp)
(replace-with-compiled comp-list)
(replace-with-compiled comp-const)
(replace-with-compiled comp-var)
(replace-with-compiled make-true-list)
(replace-with-compiled assemble)
(replace-with-compiled optimize)
(replace-with-compiled gen)
(replace-with-compiled seq)
(replace-with-compiled gen-var)
(replace-with-compiled gen-label)
(replace-with-compiled gen-set)
(replace-with-compiled in-env?)
(replace-with-compiled label?)
(replace-with-compiled args)
(replace-with-compiled arg1)
(replace-with-compiled arg2)
(replace-with-compiled is)
(replace-with-compiled set-arg1!)
(replace-with-compiled asm-first-pass)
(replace-with-compiled asm-second-pass)
(replace-with-compiled compiler)
(replace-with-compiled opcode)
(replace-with-compiled show-fn)
(replace-with-compiled comp-show)
(replace-with-compiled comp-repl)
