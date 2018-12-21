#!/bin/bash

function col_count { cat $1 | awk '{print NF}' | head -n 1; }

function col_name { cat $1 | awk "{print \$$2}" | head -n 1; }

function unique_values { cat $1 | awk "{print \$$2}" | sort | uniq | wc -l | awk '{print $1-1}'; }

function file_summary { for col in $(seq 1 `col_count $1`); do echo "$(col_name $1 $col) - $(unique_values $1 $col)"; done; }

function summary { for file in $(find . -name 'train.log.txt'); do file_summary $file; done; }


# csv versions

function col_count { cat $1 | awk -F ',' '{print NF}' | head -n 1; }

function col_name { cat $1 | awk -F ',' "{print \$$2}" | head -n 1; }

function unique_values { cat $1 | awk -F ',' "{print \$$2}" | sort | uniq | wc -l | awk '{print $1-1}'; }

function file_summary { for col in $(seq 1 `col_count $1`); do echo "$(col_name $1 $col) - $(unique_values $1 $col)"; done; }

function summary { for file in $(find . -name 'train.log.txt'); do file_summary $file; done; }
