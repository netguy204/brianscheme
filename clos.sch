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

; DESCRIPTION:
;
; This loads the tiny-clos library created by some very bright
; people at XEROX Parc.
;
; http://community.schemewiki.org/?Tiny-CLOS
;
; Interestingly, this implementation supports a meta-object-protocol
; just like real clos. It's dog slow, but it works!

(require "clos/support.sch")
(require "clos/clos.sch")
