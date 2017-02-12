#!/bin/bash

stack exec site clean && stack exec site build && stack exec site deploy
