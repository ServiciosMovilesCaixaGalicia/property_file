#!/bin/bash
MLEX_EBIN=../mlex/ebin
erl -noshell -pa ./ebin -pa $MLEX_EBIN -s property_file_test test_all quit

