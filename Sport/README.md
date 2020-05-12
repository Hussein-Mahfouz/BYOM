{\rtf1\ansi\ansicpg1252\cocoartf1671\cocoasubrtf600
{\fonttbl\f0\fswiss\fcharset0 Helvetica;\f1\froman\fcharset0 Times-Roman;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue233;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c93333;}
\paperw11900\paperh16840\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural\partightenfactor0

\f0\fs36 \cf0 Animation of Olympic Medal Data
\fs24 \
\
The gif here shows the country with the highest tally of Olympic medals in all games from 1896 to 2008.\
\
Data is available from {\field{\*\fldinst{HYPERLINK "https://docs.google.com/spreadsheets/d/1zeeZQzFoHE2j_ZrqDkVJK9eF7OH1yvg75c8S-aBcxaU/edit#gid=322436777"}}{\fldrslt 
\f1 \cf2 \expnd0\expndtw0\kerning0
\ul \ulc2 \outl0\strokewidth0 \strokec2 https://docs.google.com/spreadsheets/d/1zeeZQzFoHE2j_ZrqDkVJK9eF7OH1yvg75c8S-aBcxaU/edit#gid=322436777}}
\f1 \cf2 \expnd0\expndtw0\kerning0
\ul \ulc2 \outl0\strokewidth0 \strokec2 \
\

\f0 \cf0 \kerning1\expnd0\expndtw0 \ulnone \outl0\strokewidth0 The animation is made using the gganimate package. \
\
The transition_states() argument is added to the ggplot function to specify the column that is plotted in the animation.\
\
The title argument in labs only takes \{closest_state\}, as explained in this github {\field{\*\fldinst{HYPERLINK "https://github.com/thomasp85/gganimate/issues/252"}}{\fldrslt issue}}. This only plots the column title as the title. One way around it is to concatenate the desired title in a new column, and pass that column to the transition_states() argument. This is done in my code\
\
NOTE ABOUT ANIMATION: The data used was grouped by country and year. It turns out that all winners of team events were listed, and so team events were counted as more than one medal. Unfortunately as a result, the animation is WRONG. }