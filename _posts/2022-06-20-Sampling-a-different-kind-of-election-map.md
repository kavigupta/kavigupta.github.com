---
layout: post
title: Sampling&colon; a different kind of election "map"
comments: True
tags: draft
---

<img src="/resources/2022-06-20/header_image.jpg" width="100%">

Election maps tend to obscure a couple things that I think are interesting about American
elections.

First, about 33% of Trump supporters, and about 30% of Biden supporters lived in
precincts that voted for the other party, so show up in the other party's color on the map!

Second, the map tends to overstate the number of people who live in purely rural areas.

The combination of these two effects leads to in my view a fairly big distortion of our pictures
of blue and red America, but especially red America, which people begin to picture as entirely
middle-of-nowhere rural areas.

Instead, here's a sampling of 1000 voters and what their neighborhoods look like. This has the
benefit of giving you a picture of what
Biden's America and Trump's America look like: not two disjoint nations, but two overlapping
and intertwined distributions. You can click each dot to jump down to the voter represented
by the dot.

<iframe id="igraph" scrolling="no" style="border:none;" seamless="seamless" src="/resources/2022-06-20/graph.html" height="525" width="100%"></iframe>

Biden's voters are placed on the left, while Trump's are on the right. I have provided
an image of what their street might look like on Google Streetview, as well as the lean of
their precinct. If you click on the image, it will take you to a Google maps link for their
precinct's center, so you can see where it is.

I think, beyond everything else, one of the biggest takeaways I have is just how empty suburban
areas in the South and Midwest are. I grew up in a fairly dense suburb of LA, where lot sizes are
about a sixth of an acre and have lived in small cities ever since, so I pictured, e.g., "Georgia suburbs"
to be similar. Instead, they look a lot like what I picture when someone says "rural America".

*Technical details: to sample the voters I first figured out how many Trump and
how many Biden voters should be picked, then for each candidate I sampled precincts
randomly, weighted by how many of that candidate's voters lived in the precinct.
I excluded Kentucky from this sampling as I do not have precinct data for Kentucky.
The precinct data is from a combination of the NYT and VEST and I try to find a street with
a "street-like" address (so not a freeway or major road) inside the precinct,
whenever possible. The images should be taken as a broadstrokes vibe of the precinct
rather than something super specific.*

<style>
    * {
        box-sizing: border-box;
    }

    /* Create two equal columns that floats next to each other */
    /* .row {
        display: flex;
    }

    .column {
        flex: 50%;
    } */

    .fill {
        display: flex;
        justify-content: center;
        align-items: center;
        overflow: hidden
    }

    .fill a {
        width: 100%;
        min-height: 100%
    }


    .fill a img {
        width: 100%;
        min-height: 100%
    }

    .voter_header {
        margin-bottom: 0px;
    }

    .text {
        font-family: courier;
        text-align: center;
    }

    .dem {
        color: #26f;
    }

    .gop {
        color: #f32;
    }

    .precinct {
        font-size: 125%;
        font-weight: bold;
    }

    .voter {
        font-size: 100%;
        font-style: italic;
    }

    .coordinate {
        font-size: 100%;
    }

    .address {
        font-size: 85%;
    }

    /* table specific stuff */
    table {
        width: 100%;
    }

    table, tr, td{
        border:none;
    }


    table td,
    table td * {
        vertical-align: top;
    }

    .wider {
        width: 80vw;
        position: relative;
        left: calc(-40vw + 50%);
    }
</style>


<table class="wider">
    <tr>
        <tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-1">Voter 1's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 1 voted for Biden</div> -->
        <div class="text coordinate"> 27&deg;37'0"N 80&deg;28'21"W</div>
        <div class="text address"><i>approx.</i> 7017, 8th Street, Indian River County, Florida, 32968, United States</div>
        <div class="text gop precinct">Precinct Margin: R+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-2">Voter 2's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 2 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;6'50"N 84&deg;34'53"W</div>
        <div class="text address"><i>approx.</i> JD's Bar-B-Que, 6426, Bells Ferry Road, Southfork, Woodstock, Cherokee County, Georgia, 30189, United States</div>
        <div class="text gop precinct">Precinct Margin: R+36%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=27.616929459014408,-80.4726233676298&ll=27.616929459014408,-80.4726233676298&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/0.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.1140103,-84.5814071&ll=34.1140103,-84.5814071&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/1.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-3">Voter 3's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 3 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;41'40"N 122&deg;7'3"W</div>
        <div class="text address"><i>approx.</i> Edendale Middle School, 16160, Ashland Avenue, San Lorenzo, Alameda County, California, 94580, United States</div>
        <div class="text dem precinct">Precinct Margin: D+60%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-4">Voter 4's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 4 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;51'29"N 80&deg;4'38"W</div>
        <div class="text address"><i>approx.</i> 113, Maplewood Avenue, Fair Grove, Thomasville, Davidson County, North Carolina, 27360, United States</div>
        <div class="text gop precinct">Precinct Margin: R+65%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.69460290000001,-122.11759362927067&ll=37.69460290000001,-122.11759362927067&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/2.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.85827550092718,-80.0774561349717&ll=35.85827550092718,-80.0774561349717&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/3.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-5">Voter 5's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 5 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;37'12"N 74&deg;47'59"W</div>
        <div class="text address"><i>approx.</i> Cushetunk Mountain Nature Preserve, Yellow Trail, Readington Township, Hunterdon County, New Jersey, 08833, United States</div>
        <div class="text gop precinct">Precinct Margin: R+12%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-6">Voter 6's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 6 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;27'50"N 81&deg;1'18"W</div>
        <div class="text address"><i>approx.</i> New Hope Baptist Church, North Little Egypt Road, Lincoln County, North Carolina, 28037, United States</div>
        <div class="text gop precinct">Precinct Margin: R+42%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.62011775,-74.79982482736204&ll=40.62011775,-74.79982482736204&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/4.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.46410855,-81.0218105&ll=35.46410855,-81.0218105&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/5.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-7">Voter 7's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 7 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;44'36"N 121&deg;20'29"W</div>
        <div class="text address"><i>approx.</i> Vineyard Road, Placer County, California, 95678, United States</div>
        <div class="text gop precinct">Precinct Margin: R+6%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-8">Voter 8's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 8 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;29'12"N 81&deg;33'47"W</div>
        <div class="text address"><i>approx.</i> 1967, Joyner Pond Road, Aiken County, South Carolina, 29803, United States</div>
        <div class="text gop precinct">Precinct Margin: R+38%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.74359079167836,-121.34147624585736&ll=38.74359079167836,-121.34147624585736&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/6.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.486680390563215,-81.5632702303557&ll=33.486680390563215,-81.5632702303557&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/7.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-9">Voter 9's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 9 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;58'0"N 78&deg;50'59"W</div>
        <div class="text address"><i>approx.</i> 295, Hartford Avenue, Ellwood Park, Buffalo, Erie County, New York, 14223, United States</div>
        <div class="text dem precinct">Precinct Margin: D+32%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-10">Voter 10's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 10 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;12'23"N 93&deg;19'10"W</div>
        <div class="text address"><i>approx.</i> 2090, 130th Avenue Northwest, Oaks, Coon Rapids, Anoka County, Minnesota, 55448, United States</div>
        <div class="text dem precinct">Precinct Margin: D+4%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.9667389,-78.8497827&ll=42.9667389,-78.8497827&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/8.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.206534,-93.3195926713798&ll=45.206534,-93.3195926713798&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/9.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-11">Voter 11's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 11 voted for Biden</div> -->
        <div class="text coordinate"> 31&deg;51'53"N 81&deg;42'24"W</div>
        <div class="text address"><i>approx.</i> 1472, Hodges Road, Liberty County, Georgia, 31313, United States</div>
        <div class="text gop precinct">Precinct Margin: R+1%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-12">Voter 12's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 12 voted for Trump</div> -->
        <div class="text coordinate"> 48&deg;8'17"N 122&deg;17'10"W</div>
        <div class="text address"><i>approx.</i> 3944, Rose Road, Lake Goodwin, Snohomish County, Washington, 98292, United States</div>
        <div class="text gop precinct">Precinct Margin: R+5%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=31.86475109055729,-81.70685493242263&ll=31.86475109055729,-81.70685493242263&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/10.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=48.13819134305308,-122.2863083022413&ll=48.13819134305308,-122.2863083022413&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/11.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-13">Voter 13's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 13 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;41'7"N 71&deg;27'29"W</div>
        <div class="text address"><i>approx.</i> 124, Cowesett Road, Cowesett, Warwick, Kent County, Rhode Island, 02886, United States</div>
        <div class="text dem precinct">Precinct Margin: D+13%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-14">Voter 14's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 14 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;18'43"N 105&deg;28'29"W</div>
        <div class="text address"><i>approx.</i> 420, Meadow Rose Avenue, Gillette, Campbell County, Wyoming, 82716, United States</div>
        <div class="text gop precinct">Precinct Margin: R+83%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.68553795705287,-71.4582443917227&ll=41.68553795705287,-71.4582443917227&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/12.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.31200709300875,-105.47484452488153&ll=44.31200709300875,-105.47484452488153&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/13.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-15">Voter 15's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 15 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;5'44"N 118&deg;12'28"W</div>
        <div class="text address"><i>approx.</i> PetrolX, N Figueroa Street, Highland Park, Los Angeles, Los Angeles County, California, 90065, United States</div>
        <div class="text dem precinct">Precinct Margin: D+72%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-16">Voter 16's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 16 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;31'41"N 98&deg;39'9"W</div>
        <div class="text address"><i>approx.</i> 8422, Brixton Street, Braun Station, San Antonio, Bexar County, Texas, 78254, United States</div>
        <div class="text dem precinct">Precinct Margin: D+0%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.0958151,-118.20795728588769&ll=34.0958151,-118.20795728588769&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/14.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.528288849408945,-98.65266543898258&ll=29.528288849408945,-98.65266543898258&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/15.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-17">Voter 17's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 17 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;56'50"N 78&deg;55'36"W</div>
        <div class="text address"><i>approx.</i> 112, Grey Elm Trail, Durham, Durham County, North Carolina, 27713, United States</div>
        <div class="text dem precinct">Precinct Margin: D+70%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-18">Voter 18's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 18 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;35'8"N 82&deg;0'24"W</div>
        <div class="text address"><i>approx.</i> 110, Floradandy Road, Putnam County, Florida, 32640, United States</div>
        <div class="text gop precinct">Precinct Margin: R+33%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.9472496,-78.9269233&ll=35.9472496,-78.9269233&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/16.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.585679680036478,-82.00674481165757&ll=29.585679680036478,-82.00674481165757&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/17.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-19">Voter 19's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 19 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;7'57"N 76&deg;23'27"W</div>
        <div class="text address"><i>approx.</i> 562, Wythe Creek Road, Rivergate, Poquoson, Virginia, 23662, United States</div>
        <div class="text gop precinct">Precinct Margin: R+43%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-20">Voter 20's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 20 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;18'23"N 119&deg;23'1"W</div>
        <div class="text address"><i>approx.</i> Aviation Way, Visalia, Tulare County, California, United States</div>
        <div class="text gop precinct">Precinct Margin: R+16%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.1325538,-76.3910244827581&ll=37.1325538,-76.3910244827581&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/18.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.30639100114503,-119.3836258006222&ll=36.30639100114503,-119.3836258006222&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/19.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-21">Voter 21's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 21 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;37'47"N 95&deg;34'49"W</div>
        <div class="text address"><i>approx.</i> Cash Road, Stafford, Fort Bend County, Texas, 77477, United States</div>
        <div class="text dem precinct">Precinct Margin: D+19%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-22">Voter 22's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 22 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;55'3"N 73&deg;51'50"W</div>
        <div class="text address"><i>approx.</i> Yonkers Raceway, Trenchard Street, Dunwoodie Heights, City of Yonkers, Westchester County, New York, 10704, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.629793022925696,-95.58042542156262&ll=29.629793022925696,-95.58042542156262&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/20.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.91758655,-73.86415793184197&ll=40.91758655,-73.86415793184197&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/21.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-23">Voter 23's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 23 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;27'18"N 80&deg;3'58"W</div>
        <div class="text address"><i>approx.</i> 3411, Chartiers Avenue, Sheraden, Pittsburgh, Allegheny County, Pennsylvania, 15204, United States</div>
        <div class="text dem precinct">Precinct Margin: D+37%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-24">Voter 24's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 24 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;37'0"N 73&deg;58'52"W</div>
        <div class="text address"><i>approx.</i> 2113, 62nd Street, Brooklyn, Kings County, City of New York, New York, 11204, United States</div>
        <div class="text dem precinct">Precinct Margin: D+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.45512017407949,-80.06636484210111&ll=40.45512017407949,-80.06636484210111&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/22.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.61678965,-73.98129176876967&ll=40.61678965,-73.98129176876967&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/23.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-25">Voter 25's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 25 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;4'24"N 80&deg;40'21"W</div>
        <div class="text address"><i>approx.</i> 31, Jenna Way, Royal Oak Estates, Wheeling, Ohio County, West Virginia, 26003, United States</div>
        <div class="text gop precinct">Precinct Margin: R+21%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-26">Voter 26's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 26 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;57'36"N 77&deg;53'59"W</div>
        <div class="text address"><i>approx.</i> 1199, South Old Carriage Road, Westry, Rocky Mount, Nash County, North Carolina, 27804, United States</div>
        <div class="text gop precinct">Precinct Margin: R+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.073525282591866,-80.6727688225307&ll=40.073525282591866,-80.6727688225307&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/24.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.9601,-77.899893&ll=35.9601,-77.899893&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/25.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-27">Voter 27's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 27 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;59'26"N 94&deg;16'14"W</div>
        <div class="text address"><i>approx.</i> 2898, Southwest 3rd Street Terrace, Blue Springs, Jackson County, Missouri, 64014, United States</div>
        <div class="text gop precinct">Precinct Margin: R+17%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-28">Voter 28's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 28 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;49'12"N 90&deg;26'36"W</div>
        <div class="text address"><i>approx.</i> Voight Road, Town of Fifield, Price County, Wisconsin, 54524, United States</div>
        <div class="text gop precinct">Precinct Margin: R+28%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.990607,-94.270564&ll=38.990607,-94.270564&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/26.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.82003288639916,-90.44358155696825&ll=45.82003288639916,-90.44358155696825&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/27.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-29">Voter 29's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 29 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;36'15"N 94&deg;11'32"W</div>
        <div class="text address"><i>approx.</i> Maple Avenue, Sauk Rapids, Benton County, Minnesota, 5377, United States</div>
        <div class="text gop precinct">Precinct Margin: R+13%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-30">Voter 30's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 30 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;17'16"N 85&deg;47'38"W</div>
        <div class="text address"><i>approx.</i> 99, EMS C28E Lane, Island Park, Kosciusko County, Indiana, 46582, United States</div>
        <div class="text gop precinct">Precinct Margin: R+54%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.6042872,-94.1923431&ll=45.6042872,-94.1923431&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/28.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.287796,-85.794124&ll=41.287796,-85.794124&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/29.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-31">Voter 31's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 31 voted for Biden</div> -->
        <div class="text coordinate"> 47&deg;31'14"N 122&deg;36'11"W</div>
        <div class="text address"><i>approx.</i> 3577, Lund Avenue Southeast, Parkwood, Conifer Park, Kitsap County, Washington, 98366, United States</div>
        <div class="text dem precinct">Precinct Margin: D+7%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-32">Voter 32's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 32 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;52'3"N 86&deg;28'27"W</div>
        <div class="text address"><i>approx.</i> 4386, Pretoria Run, Butler Estates, Murfreesboro, Rutherford County, Tennessee, 37128, United States</div>
        <div class="text gop precinct">Precinct Margin: R+14%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.52078504553212,-122.60321240418212&ll=47.52078504553212,-122.60321240418212&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/30.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.867569491933956,-86.47430013897083&ll=35.867569491933956,-86.47430013897083&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/31.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-33">Voter 33's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 33 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;18'7"N 75&deg;21'37"W</div>
        <div class="text address"><i>approx.</i> 515, Meetinghouse Road, Franconia, Franconia Township, Montgomery County, Pennsylvania, 18964, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-34">Voter 34's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 34 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;17'23"N 83&deg;57'38"W</div>
        <div class="text address"><i>approx.</i> 247, Jefferson Street, Quincy, Miami Township, Logan County, Ohio, 43343, United States</div>
        <div class="text gop precinct">Precinct Margin: R+71%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.3021689,-75.3603972&ll=40.3021689,-75.3603972&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/32.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.28989823221561,-83.96071869141004&ll=40.28989823221561,-83.96071869141004&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/33.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-35">Voter 35's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 35 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;48'51"N 74&deg;29'47"W</div>
        <div class="text address"><i>approx.</i> 116, Lake Road, Butterworth Farms, Morris Plains, Morris Township, Morris County, New Jersey, 07960, United States</div>
        <div class="text dem precinct">Precinct Margin: D+25%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-36">Voter 36's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 36 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;49'14"N 71&deg;14'7"W</div>
        <div class="text address"><i>approx.</i> 96, High Street, Salem, Rockingham County, New Hampshire, 03079, United States</div>
        <div class="text gop precinct">Precinct Margin: R+13%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.81437743581967,-74.49657899400687&ll=40.81437743581967,-74.49657899400687&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/34.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.820780419160805,-71.23545693412566&ll=42.820780419160805,-71.23545693412566&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/35.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-37">Voter 37's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 37 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;31'27"N 122&deg;44'41"W</div>
        <div class="text address"><i>approx.</i> Pond Trail, Santa Rosa, Sonoma County, California, 95439, United States</div>
        <div class="text dem precinct">Precinct Margin: D+42%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-38">Voter 38's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 38 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;52'14"N 118&deg;22'25"W</div>
        <div class="text address"><i>approx.</i> 2112, Carnegie Lane, El Nido, Redondo Beach, Los Angeles County, California, 90278, United States</div>
        <div class="text dem precinct">Precinct Margin: D+47%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.5242659,-122.7448476&ll=38.5242659,-122.7448476&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/36.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.87067635,-118.37370954979346&ll=33.87067635,-118.37370954979346&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/37.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-39">Voter 39's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 39 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;44'51"N 104&deg;58'16"W</div>
        <div class="text address"><i>approx.</i> 1375, East 20th Avenue, City Park West, Denver, Colorado, 80205, United States</div>
        <div class="text dem precinct">Precinct Margin: D+75%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-40">Voter 40's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 40 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;52'56"N 117&deg;52'17"W</div>
        <div class="text address"><i>approx.</i> 451, West Madison Avenue, Old Town Placentia, Placentia, Orange County, California, 92870, United States</div>
        <div class="text gop precinct">Precinct Margin: R+1%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.7476583,-104.97119185467639&ll=39.7476583,-104.97119185467639&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/38.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.88233235,-117.87159436723488&ll=33.88233235,-117.87159436723488&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/39.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-41">Voter 41's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 41 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;23'35"N 111&deg;3'40"W</div>
        <div class="text address"><i>approx.</i> 4513, West Mesquital del Oro, Pima County, Arizona, 85742, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-42">Voter 42's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 42 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;26'49"N 97&deg;46'23"W</div>
        <div class="text address"><i>approx.</i> 8405, Foxhound Trail, Hunter's Chase, Jollyville, Austin, Williamson County, Texas, 78729, United States</div>
        <div class="text dem precinct">Precinct Margin: D+44%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.3930752812397,-111.06137392174449&ll=32.3930752812397,-111.06137392174449&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/40.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.4471225,-97.7731111118421&ll=30.4471225,-97.7731111118421&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/41.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-43">Voter 43's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 43 voted for Biden</div> -->
        <div class="text coordinate"> 28&deg;1'37"N 82&deg;35'9"W</div>
        <div class="text address"><i>approx.</i> 8911, Saddletree Way, Hillsborough County, Florida, 33635, United States</div>
        <div class="text dem precinct">Precinct Margin: D+22%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-44">Voter 44's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 44 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;10'15"N 79&deg;2'29"W</div>
        <div class="text address"><i>approx.</i> 505, Cayuga Street, Lewiston Heights, Village of Lewiston, Town of Lewiston, Niagara County, New York, 14092, United States</div>
        <div class="text dem precinct">Precinct Margin: D+4%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=28.02701925,-82.58586828622094&ll=28.02701925,-82.58586828622094&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/42.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.1708634,-79.0413966&ll=43.1708634,-79.0413966&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/43.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-45">Voter 45's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 45 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;34'57"N 93&deg;6'52"W</div>
        <div class="text address"><i>approx.</i> 1705, Savage Road, Mincy, Taney County, Missouri, 65679, United States</div>
        <div class="text gop precinct">Precinct Margin: R+62%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-46">Voter 46's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 46 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;3'4"N 118&deg;6'46"W</div>
        <div class="text address"><i>approx.</i> 1057, South Orange Avenue, Monterey Park, Los Angeles County, California, 91755, United States</div>
        <div class="text dem precinct">Precinct Margin: D+37%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.582675,-93.114684&ll=36.582675,-93.114684&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/44.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.0512606470201,-118.11288745800547&ll=34.0512606470201,-118.11288745800547&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/45.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-47">Voter 47's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 47 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;31'36"N 79&deg;40'53"W</div>
        <div class="text address"><i>approx.</i> 11247, East Meadow Road, Green Acres, Draper Village, Rockingham County, North Carolina, 27288, United States</div>
        <div class="text gop precinct">Precinct Margin: R+26%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-48">Voter 48's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 48 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;43'53"N 86&deg;13'54"W</div>
        <div class="text address"><i>approx.</i> 7482, East 250 S, Cass County, Indiana, 46994, United States</div>
        <div class="text gop precinct">Precinct Margin: R+59%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.52670733954369,-79.68154912026253&ll=36.52670733954369,-79.68154912026253&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/46.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.731449688459946,-86.23174342005473&ll=40.731449688459946,-86.23174342005473&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/47.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-49">Voter 49's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 49 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;58'28"N 83&deg;57'18"W</div>
        <div class="text address"><i>approx.</i> 411, Grenier Terrace Northeast, Gwinnett County, Georgia, 30045, United States</div>
        <div class="text dem precinct">Precinct Margin: D+53%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-50">Voter 50's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 50 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;55'57"N 95&deg;53'29"W</div>
        <div class="text address"><i>approx.</i> 7421, Southwest Docking Road, Shawnee County, Kansas, 66402, United States</div>
        <div class="text gop precinct">Precinct Margin: R+37%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.974548627957645,-83.95504453240382&ll=33.974548627957645,-83.95504453240382&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/48.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.932578315243305,-95.89163568221328&ll=38.932578315243305,-95.89163568221328&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/49.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-51">Voter 51's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 51 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;2'23"N 118&deg;13'50"W</div>
        <div class="text address"><i>approx.</i> Santa Fe Avenue, Arts District, Downtown, Los Angeles, Los Angeles County, California, 90021, United States</div>
        <div class="text dem precinct">Precinct Margin: D+75%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-52">Voter 52's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 52 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;16'32"N 87&deg;40'5"W</div>
        <div class="text address"><i>approx.</i> 900, East 22nd Avenue, Gulf Shores, Baldwin County, Alabama, 36542, United States</div>
        <div class="text gop precinct">Precinct Margin: R+54%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.03994166268687,-118.23069969598706&ll=34.03994166268687,-118.23069969598706&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/50.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.27556001058193,-87.6682192593824&ll=30.27556001058193,-87.6682192593824&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/51.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-53">Voter 53's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 53 voted for Biden</div> -->
        <div class="text coordinate"> 43&deg;4'39"N 87&deg;53'58"W</div>
        <div class="text address"><i>approx.</i> Roverwest Dog Exercise Area, North Bremen Street, Riverwest, Milwaukee, Milwaukee County, Wisconsin, 53212, United States</div>
        <div class="text dem precinct">Precinct Margin: D+77%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-54">Voter 54's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 54 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;43'5"N 84&deg;20'22"W</div>
        <div class="text address"><i>approx.</i> 3675, North Hope Road, Lincoln Township, Midland County, Michigan, 48642, United States</div>
        <div class="text gop precinct">Precinct Margin: R+29%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.077629849999994,-87.89961185000001&ll=43.077629849999994,-87.89961185000001&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/52.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.718111156399985,-84.3395226255759&ll=43.718111156399985,-84.3395226255759&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/53.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-55">Voter 55's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 55 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;35'57"N 75&deg;23'57"W</div>
        <div class="text address"><i>approx.</i> 603, Sayre Street, Fountain Hill, Lehigh County, Pennsylvania, 18015, United States</div>
        <div class="text dem precinct">Precinct Margin: D+30%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-56">Voter 56's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 56 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;46'11"N 92&deg;7'21"W</div>
        <div class="text address"><i>approx.</i> Huddleston Road, Cleveland County, Arkansas, 71660, United States</div>
        <div class="text gop precinct">Precinct Margin: R+56%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.599185000000006,-75.39933630612245&ll=40.599185000000006,-75.39933630612245&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/54.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.769929,-92.122545&ll=33.769929,-92.122545&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/55.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-57">Voter 57's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 57 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;9'25"N 96&deg;2'24"W</div>
        <div class="text address"><i>approx.</i> Rolling Hills Church, North Monroe Street, Papillion, Sarpy County, Nebraska, 68046, United States</div>
        <div class="text gop precinct">Precinct Margin: R+12%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-58">Voter 58's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 58 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;41'33"N 83&deg;36'7"W</div>
        <div class="text address"><i>approx.</i> Telephone Exchange Central Office, 2414, West Sylvania Avenue, Deveaux, Fitch, Toledo, Lucas County, Ohio, 43560, United States</div>
        <div class="text dem precinct">Precinct Margin: D+15%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.1570707,-96.0400486&ll=41.1570707,-96.0400486&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/56.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.69263395,-83.60200594092458&ll=41.69263395,-83.60200594092458&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/57.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-59">Voter 59's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 59 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;13'41"N 121&deg;59'12"W</div>
        <div class="text address"><i>approx.</i> 18, Ellenwood Avenue, Los Gatos, Santa Clara County, California, 95030, United States</div>
        <div class="text dem precinct">Precinct Margin: D+48%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-60">Voter 60's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 60 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;19'45"N 111&deg;48'1"W</div>
        <div class="text address"><i>approx.</i> 12911, South 131st Street, Gilbert, Maricopa County, Arizona, 85233, United States</div>
        <div class="text gop precinct">Precinct Margin: R+3%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.228248,-121.986928&ll=37.228248,-121.986928&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/58.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.32931825819109,-111.80043198488846&ll=33.32931825819109,-111.80043198488846&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/59.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-61">Voter 61's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 61 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;40'43"N 121&deg;32'32"W</div>
        <div class="text address"><i>approx.</i> 1600, Tucker Road, Hood River County, Oregon, 97031, United States</div>
        <div class="text dem precinct">Precinct Margin: D+19%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-62">Voter 62's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 62 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;23'10"N 91&deg;56'6"W</div>
        <div class="text address"><i>approx.</i> 1099, East G Land Road, St. Martin Parish, Louisiana, 70512, United States</div>
        <div class="text gop precinct">Precinct Margin: R+66%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.678763,-121.542229&ll=45.678763,-121.542229&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/60.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.38612,-91.935079&ll=30.38612,-91.935079&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/61.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-63">Voter 63's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 63 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;23'24"N 89&deg;5'13"W</div>
        <div class="text address"><i>approx.</i> 3225, 20th Avenue, Gulfport, Harrison County, Mississippi, 39501, United States</div>
        <div class="text dem precinct">Precinct Margin: D+41%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-64">Voter 64's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 64 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;4'5"N 82&deg;59'18"W</div>
        <div class="text address"><i>approx.</i> 1319, Culbertson Road, Greene County, Tennessee, 37743, United States</div>
        <div class="text gop precinct">Precinct Margin: R+70%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.39000925,-89.08696507512525&ll=30.39000925,-89.08696507512525&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/62.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.0681586377312,-82.98854438555779&ll=36.0681586377312,-82.98854438555779&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/63.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-65">Voter 65's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 65 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;49'48"N 71&deg;23'27"W</div>
        <div class="text address"><i>approx.</i> 15, Taber Avenue, Wayland, Providence, Providence County, Rhode Island, 02906, United States</div>
        <div class="text dem precinct">Precinct Margin: D+80%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-66">Voter 66's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 66 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;8'58"N 96&deg;55'38"W</div>
        <div class="text address"><i>approx.</i> 7628, Snug Harbor Circle, Denton County, Texas, 75036, United States</div>
        <div class="text dem precinct">Precinct Margin: D+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.830119,-71.391035&ll=41.830119,-71.391035&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/64.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.149642551853034,-96.92742979038037&ll=33.149642551853034,-96.92742979038037&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/65.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-67">Voter 67's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 67 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;45'28"N 84&deg;10'11"W</div>
        <div class="text address"><i>approx.</i> 41, Drummer Avenue, Saint Annes Hill Historic District, Dayton, Montgomery County, Ohio, 45403, United States</div>
        <div class="text dem precinct">Precinct Margin: D+42%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-68">Voter 68's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 68 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;2'54"N 87&deg;22'31"W</div>
        <div class="text address"><i>approx.</i> 582, West Taylor Avenue, Chandler, Warrick County, Indiana, 47610, United States</div>
        <div class="text gop precinct">Precinct Margin: R+13%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.75804015,-84.16982064999999&ll=39.75804015,-84.16982064999999&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/66.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.04833748979592,-87.37549848979592&ll=38.04833748979592,-87.37549848979592&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/67.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-69">Voter 69's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 69 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;33'27"N 112&deg;20'22"W</div>
        <div class="text address"><i>approx.</i> 7210, East Sienna Springs Lane, Prescott Valley, Yavapai County, Arizona, 86314, United States</div>
        <div class="text gop precinct">Precinct Margin: R+39%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-70">Voter 70's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 70 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;40'10"N 117&deg;17'40"W</div>
        <div class="text address"><i>approx.</i> 31643, Chaparral Way, Lake Elsinore, Riverside County, California, 92532, United States</div>
        <div class="text dem precinct">Precinct Margin: D+1%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.55765833576843,-112.33971068446161&ll=34.55765833576843,-112.33971068446161&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/68.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.66972137275206,-117.29466659293925&ll=33.66972137275206,-117.29466659293925&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/69.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-71">Voter 71's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 71 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;4'15"N 93&deg;22'46"W</div>
        <div class="text address"><i>approx.</i> 6377, Sumter Avenue North, Brooklyn Park, Hennepin County, Minnesota, 55428, United States</div>
        <div class="text dem precinct">Precinct Margin: D+42%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-72">Voter 72's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 72 voted for Trump</div> -->
        <div class="text coordinate"> 26&deg;25'54"N 81&deg;44'18"W</div>
        <div class="text address"><i>approx.</i> 21008, Torre Del Lago Street, Bella Terra, Lee County, Florida, 33928, United States</div>
        <div class="text gop precinct">Precinct Margin: R+31%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.070969179403555,-93.37969337137146&ll=45.070969179403555,-93.37969337137146&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/70.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.431722630030308,-81.73833589693645&ll=26.431722630030308,-81.73833589693645&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/71.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-73">Voter 73's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 73 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;54'36"N 78&deg;55'35"W</div>
        <div class="text address"><i>approx.</i> 604, Forge Road, Crooked Creek, Durham, Durham County, North Carolina, 27713, United States</div>
        <div class="text dem precinct">Precinct Margin: D+74%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-74">Voter 74's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 74 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;34'27"N 72&deg;23'44"W</div>
        <div class="text address"><i>approx.</i> 232, Cato Corner Road, North Westchester, Colchester, New London County, Connecticut, 06415, United States</div>
        <div class="text dem precinct">Precinct Margin: D+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.910266,-78.9266628&ll=35.910266,-78.9266628&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/72.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.57421331729616,-72.39563430561194&ll=41.57421331729616,-72.39563430561194&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/73.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-75">Voter 75's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 75 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;46'50"N 78&deg;37'33"W</div>
        <div class="text address"><i>approx.</i> E Edenton St at Idlewild Ave, East Edenton Street, Idlewild, Seaboard Station, Raleigh, Wake County, North Carolina, 27601, United States</div>
        <div class="text dem precinct">Precinct Margin: D+88%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-76">Voter 76's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 76 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;48'43"N 76&deg;23'41"W</div>
        <div class="text address"><i>approx.</i> 308, Snead Fairway, Manor View, Portsmouth City, Virginia, 23701, United States</div>
        <div class="text gop precinct">Precinct Margin: R+22%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.7808226,-78.6259745&ll=35.7808226,-78.6259745&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/74.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.812191049999996,-76.39491626014394&ll=36.812191049999996,-76.39491626014394&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/75.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-77">Voter 77's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 77 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;36'15"N 83&deg;6'35"W</div>
        <div class="text address"><i>approx.</i> 5768, John R Road, Troy, Oakland County, Michigan, 48085, United States</div>
        <div class="text dem precinct">Precinct Margin: D+7%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-78">Voter 78's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 78 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;17'42"N 92&deg;41'53"W</div>
        <div class="text address"><i>approx.</i> 263rd Street, Town of Farmington, Polk County, Wisconsin, 54020, United States</div>
        <div class="text gop precinct">Precinct Margin: R+35%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.604234657718116,-83.10987870469798&ll=42.604234657718116,-83.10987870469798&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/76.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.2952519,-92.6980692&ll=45.2952519,-92.6980692&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/77.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-79">Voter 79's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 79 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;54'1"N 77&deg;0'55"W</div>
        <div class="text address"><i>approx.</i> 303, H Street Northwest, Mount Vernon Square, Washington, District of Columbia, 20548, United States</div>
        <div class="text dem precinct">Precinct Margin: D+88%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-80">Voter 80's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 80 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;36'59"N 117&deg;51'16"W</div>
        <div class="text address"><i>approx.</i> 2101, Yacht Wanderer, Newport Beach, Orange County, California, 92660, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.9004171,-77.0155109&ll=38.9004171,-77.0155109&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/78.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.616419699999994,-117.85470045882968&ll=33.616419699999994,-117.85470045882968&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/79.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-81">Voter 81's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 81 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;18'37"N 88&deg;51'30"W</div>
        <div class="text address"><i>approx.</i> 10810, Caledonia Road, Belvidere Township, Boone County, Illinois, 61008, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-82">Voter 82's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 82 voted for Trump</div> -->
        <div class="text coordinate"> 31&deg;53'13"N 82&deg;34'4"W</div>
        <div class="text address"><i>approx.</i> 299, Joe Hester Road, Hazlehurst, Jeff Davis County, Georgia, 31539, United States</div>
        <div class="text gop precinct">Precinct Margin: R+73%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.310340972659375,-88.8585573109462&ll=42.310340972659375,-88.8585573109462&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/80.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=31.887075,-82.567818&ll=31.887075,-82.567818&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/81.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-83">Voter 83's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 83 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;6'11"N 85&deg;6'22"W</div>
        <div class="text address"><i>approx.</i> 2911, Rolston Street, Fort Wayne, Allen County, Indiana, 46805, United States</div>
        <div class="text dem precinct">Precinct Margin: D+5%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-84">Voter 84's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 84 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;45'51"N 120&deg;50'19"W</div>
        <div class="text address"><i>approx.</i> 487, Pedersen Road, Oakdale, Stanislaus County, California, 95361, United States</div>
        <div class="text gop precinct">Precinct Margin: R+31%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.1030945,-85.1063397&ll=41.1030945,-85.1063397&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/82.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.76440842910331,-120.83882012325&ll=37.76440842910331,-120.83882012325&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/83.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-85">Voter 85's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 85 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;48'29"N 117&deg;13'35"W</div>
        <div class="text address"><i>approx.</i> 4809, Beryl Way, Pacific Beach, San Diego, San Diego County, California, 92109, United States</div>
        <div class="text dem precinct">Precinct Margin: D+41%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-86">Voter 86's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 86 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;2'38"N 121&deg;12'12"W</div>
        <div class="text address"><i>approx.</i> 61762, Teal Road, Deschutes County, Oregon, 97702, United States</div>
        <div class="text gop precinct">Precinct Margin: R+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.808221,-117.226624&ll=32.808221,-117.226624&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/84.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.04391201784705,-121.20349282331507&ll=44.04391201784705,-121.20349282331507&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/85.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-87">Voter 87's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 87 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;25'41"N 87&deg;40'44"W</div>
        <div class="text address"><i>approx.</i> 650, Clover Lane, Park Forest South, University Park, Will County, Illinois, 60484, United States</div>
        <div class="text dem precinct">Precinct Margin: D+83%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-88">Voter 88's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 88 voted for Trump</div> -->
        <div class="text coordinate"> 27&deg;25'51"N 82&deg;34'21"W</div>
        <div class="text address"><i>approx.</i> 7, U Avenue, Bayshore Windmill Village, Manatee County, Florida, 34207, United States</div>
        <div class="text gop precinct">Precinct Margin: R+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.42811136846733,-87.67895715927897&ll=41.42811136846733,-87.67895715927897&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/86.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=27.430907,-82.572606&ll=27.430907,-82.572606&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/87.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-89">Voter 89's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 89 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;40'47"N 121&deg;2'41"W</div>
        <div class="text address"><i>approx.</i> 2728, Sparks Way, Modesto, Stanislaus County, California, 95350, United States</div>
        <div class="text dem precinct">Precinct Margin: D+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-90">Voter 90's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 90 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;36'48"N 86&deg;40'41"W</div>
        <div class="text address"><i>approx.</i> 1096, Springville Road, Spring Lake Estates, Birmingham, Jefferson County, Alabama, 35215, United States</div>
        <div class="text dem precinct">Precinct Margin: D+65%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.679943093959736,-121.04472855033558&ll=37.679943093959736,-121.04472855033558&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/88.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.613600068807074,-86.67829093119293&ll=33.613600068807074,-86.67829093119293&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/89.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-91">Voter 91's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 91 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;56'20"N 96&deg;38'43"W</div>
        <div class="text address"><i>approx.</i> 1375, Wagon Wheel Road, Garland, Dallas County, Texas, 75040, United States</div>
        <div class="text gop precinct">Precinct Margin: R+2%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-92">Voter 92's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 92 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;55'47"N 104&deg;44'58"W</div>
        <div class="text address"><i>approx.</i> 4239, Ginger Cove Place, Newport Heights, Colorado Springs, El Paso County, Colorado, 80923, United States</div>
        <div class="text gop precinct">Precinct Margin: R+17%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.93916221212121,-96.64549581818183&ll=32.93916221212121,-96.64549581818183&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/90.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.92998505,-104.7495406213476&ll=38.92998505,-104.7495406213476&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/91.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-93">Voter 93's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 93 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;45'35"N 73&deg;52'26"W</div>
        <div class="text address"><i>approx.</i> White Course, Central Court Trailer Park, Lisha Kill, Town of Colonie, Albany County, New York, 12205-4221, United States</div>
        <div class="text gop precinct">Precinct Margin: R+2%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-94">Voter 94's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 94 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;28'14"N 96&deg;0'54"W</div>
        <div class="text address"><i>approx.</i> 1605, County Road 2421, Van Zandt County, Texas, 75103, United States</div>
        <div class="text gop precinct">Precinct Margin: R+78%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.75997375,-73.87395809457118&ll=42.75997375,-73.87395809457118&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/92.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.47072307664095,-96.01524964092886&ll=32.47072307664095,-96.01524964092886&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/93.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-95">Voter 95's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 95 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;9'57"N 73&deg;20'47"W</div>
        <div class="text address"><i>approx.</i> 99, Brush Hill Road, Brookside, Great Barrington, Berkshire County, Massachusetts, 01230, United States</div>
        <div class="text dem precinct">Precinct Margin: D+67%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-96">Voter 96's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 96 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;2'10"N 73&deg;45'59"W</div>
        <div class="text address"><i>approx.</i> 45, Barker Avenue, City of White Plains, Westchester County, New York, 10601, United States</div>
        <div class="text dem precinct">Precinct Margin: D+49%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.16593855,-73.346459087272&ll=42.16593855,-73.346459087272&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/94.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.0363861,-73.7665813&ll=41.0363861,-73.7665813&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/95.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-97">Voter 97's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 97 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;44'48"N 79&deg;20'4"W</div>
        <div class="text address"><i>approx.</i> 197, Evans Chapel Road, Town of Siler City, Chatham County, North Carolina, 27344, United States</div>
        <div class="text gop precinct">Precinct Margin: R+14%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-98">Voter 98's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 98 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;52'41"N 121&deg;16'24"W</div>
        <div class="text address"><i>approx.</i> 500, Snapdragon Lane, Lincoln, Placer County, California, 95648, United States</div>
        <div class="text dem precinct">Precinct Margin: D+0%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.7468835,-79.3346498&ll=35.7468835,-79.3346498&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/96.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.878331,-121.273474&ll=38.878331,-121.273474&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/97.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-99">Voter 99's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 99 voted for Biden</div> -->
        <div class="text coordinate"> 43&deg;46'33"N 87&deg;57'28"W</div>
        <div class="text address"><i>approx.</i> Woodland Road, Town of Plymouth, Sheboygan County, Wisconsin, 53085, United States</div>
        <div class="text gop precinct">Precinct Margin: R+31%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-100">Voter 100's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 100 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;31'25"N 73&deg;4'13"W</div>
        <div class="text address"><i>approx.</i> 119, Taft Pointe, Waterbury, New Haven County, Connecticut, 06708, United States</div>
        <div class="text dem precinct">Precinct Margin: D+0%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.77605406518609,-87.95797012902658&ll=43.77605406518609,-87.95797012902658&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/98.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.5238149,-73.0705&ll=41.5238149,-73.0705&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/99.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-101">Voter 101's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 101 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;38'48"N 73&deg;53'54"W</div>
        <div class="text address"><i>approx.</i> 727, East 101st Street, Brooklyn, Kings County, City of New York, New York, 11236, United States</div>
        <div class="text dem precinct">Precinct Margin: D+92%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-102">Voter 102's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 102 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;44'33"N 86&deg;43'35"W</div>
        <div class="text address"><i>approx.</i> 104, Stoneridge Circle, Stone Ridge, Madison, Madison County, Alabama, 35758, United States</div>
        <div class="text gop precinct">Precinct Margin: R+28%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.64672785,-73.8984944&ll=40.64672785,-73.8984944&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/100.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.7426204,-86.72640345573043&ll=34.7426204,-86.72640345573043&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/101.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-103">Voter 103's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 103 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;34'14"N 111&deg;45'38"W</div>
        <div class="text address"><i>approx.</i> 9223, North Crown Ridge, Fountain Hills, Maricopa County, Arizona, 85268, United States</div>
        <div class="text gop precinct">Precinct Margin: R+25%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-104">Voter 104's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 104 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;9'44"N 92&deg;10'22"W</div>
        <div class="text address"><i>approx.</i> 258, 5th Street Southwest, Plainview, Wabasha County, Minnesota, 55964, United States</div>
        <div class="text gop precinct">Precinct Margin: R+38%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.57077845,-111.76058465815579&ll=33.57077845,-111.76058465815579&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/102.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.162493408163265,-92.17279528571429&ll=44.162493408163265,-92.17279528571429&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/103.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-105">Voter 105's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 105 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;6'47"N 117&deg;19'14"W</div>
        <div class="text address"><i>approx.</i> Avenida Encinas, Carlsbad, San Diego County, California, 92009-1114, United States</div>
        <div class="text dem precinct">Precinct Margin: D+18%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-106">Voter 106's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 106 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;44'29"N 118&deg;20'40"W</div>
        <div class="text address"><i>approx.</i> Mariposa Trail, Rancho Palos Verdes, Los Angeles County, California, 90275, United States</div>
        <div class="text dem precinct">Precinct Margin: D+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.11331984876724,-117.32082382497707&ll=33.11331984876724,-117.32082382497707&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/104.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.7415456,-118.344637&ll=33.7415456,-118.344637&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/105.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-107">Voter 107's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 107 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;45'13"N 74&deg;0'24"W</div>
        <div class="text address"><i>approx.</i> West 29th Street, Manhattan Community Board 4, Manhattan, New York County, City of New York, New York, 10001, United States</div>
        <div class="text dem precinct">Precinct Margin: D+75%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-108">Voter 108's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 108 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;41'50"N 113&deg;3'4"W</div>
        <div class="text address"><i>approx.</i> 13th Hole Trail, Cedar City, Iron County, Utah, 83721, United States</div>
        <div class="text gop precinct">Precinct Margin: R+53%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.75367899753868,-74.00673676003511&ll=40.75367899753868,-74.00673676003511&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/106.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.6973092,-113.0512432&ll=37.6973092,-113.0512432&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/107.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-109">Voter 109's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 109 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;42'40"N 87&deg;6'30"W</div>
        <div class="text address"><i>approx.</i> 11744, Nuclear Plant Road, Browns Ferry Nuclear Power Plant, Limestone County, Alabama, 35611, United States</div>
        <div class="text gop precinct">Precinct Margin: R+35%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-110">Voter 110's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 110 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;15'35"N 85&deg;9'48"W</div>
        <div class="text address"><i>approx.</i> 168, Griffin Street, Hell's Hollow, Rome, Floyd County, Georgia, 30161, United States</div>
        <div class="text gop precinct">Precinct Margin: R+2%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.711253400722846,-87.10856558128906&ll=34.711253400722846,-87.10856558128906&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/108.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.25973512244898,-85.16345597959184&ll=34.25973512244898,-85.16345597959184&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/109.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-111">Voter 111's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 111 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;57'36"N 118&deg;9'32"W</div>
        <div class="text address"><i>approx.</i> 6032, Gotham Street, Bell Gardens, Los Angeles County, California, 90201, United States</div>
        <div class="text dem precinct">Precinct Margin: D+66%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-112">Voter 112's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 112 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;8'6"N 77&deg;32'17"W</div>
        <div class="text address"><i>approx.</i> 2615, East Avenue, Town/Village of East Rochester, Monroe County, New York, 14610, United States</div>
        <div class="text dem precinct">Precinct Margin: D+30%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.9602690538373,-118.15889618725113&ll=33.9602690538373,-118.15889618725113&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/110.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.1350427,-77.5381508&ll=43.1350427,-77.5381508&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/111.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-113">Voter 113's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 113 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;50'5"N 74&deg;5'45"W</div>
        <div class="text address"><i>approx.</i> 278, Garden Street, East Rutherford, Bergen County, New Jersey, 07073, United States</div>
        <div class="text dem precinct">Precinct Margin: D+11%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-114">Voter 114's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 114 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;23'43"N 98&deg;43'54"W</div>
        <div class="text address"><i>approx.</i> Mulberry Creek, Stoney Creek, Bexar County, Texas, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.83497942372881,-74.09599389830508&ll=40.83497942372881,-74.09599389830508&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/112.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.395479754748525,-98.73194427801448&ll=29.395479754748525,-98.73194427801448&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/113.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-115">Voter 115's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 115 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;20'1"N 73&deg;47'7"W</div>
        <div class="text address"><i>approx.</i> 3686, Curry Street, Jefferson Valley, Jefferson Valley-Yorktown, Westchester County, New York, 10598, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-116">Voter 116's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 116 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;53'20"N 72&deg;55'6"W</div>
        <div class="text address"><i>approx.</i> 296, Barbourtown Road, Canton, Hartford County, Connecticut, 06019, United States</div>
        <div class="text dem precinct">Precinct Margin: D+18%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.3336976,-73.7853214&ll=41.3336976,-73.7853214&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/114.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.88910748838584,-72.91843212342519&ll=41.88910748838584,-72.91843212342519&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/115.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-117">Voter 117's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 117 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;48'19"N 95&deg;23'52"W</div>
        <div class="text address"><i>approx.</i> Hamilton Middle School, East 21st Street, Houston, Harris County, Texas, 77008, United States</div>
        <div class="text dem precinct">Precinct Margin: D+38%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-118">Voter 118's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 118 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;9'31"N 89&deg;59'56"W</div>
        <div class="text address"><i>approx.</i> 765, Hawthorne Street, Memphis, Shelby County, Tennessee, 38107, United States</div>
        <div class="text dem precinct">Precinct Margin: D+63%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.80541285,-95.39798852958799&ll=29.80541285,-95.39798852958799&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/116.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.15865746668126,-89.99897524442864&ll=35.15865746668126,-89.99897524442864&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/117.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-119">Voter 119's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 119 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;14'4"N 81&deg;40'47"W</div>
        <div class="text address"><i>approx.</i> 3472, 3rd Avenue Northwest, Golden Gate Estates, Collier County, Florida, 34120, United States</div>
        <div class="text gop precinct">Precinct Margin: R+34%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-120">Voter 120's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 120 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;20'49"N 76&deg;47'57"W</div>
        <div class="text address"><i>approx.</i> 9041, Old Court Road, Randallstown, Baltimore County, Maryland, 21244, United States</div>
        <div class="text dem precinct">Precinct Margin: D+81%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.234456220194897,-81.67987766512466&ll=26.234456220194897,-81.67987766512466&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/118.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.3471361,-76.79929217019867&ll=39.3471361,-76.79929217019867&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/119.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-121">Voter 121's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 121 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;23'28"N 84&deg;21'40"W</div>
        <div class="text address"><i>approx.</i> 7983, Princeton Road, The Trails of Four Bridges, Jericho, Liberty Township, Butler County, Ohio, 45044, United States</div>
        <div class="text gop precinct">Precinct Margin: R+26%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-122">Voter 122's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 122 voted for Trump</div> -->
        <div class="text coordinate"> 25&deg;45'21"N 80&deg;13'8"W</div>
        <div class="text address"><i>approx.</i> 1435, Southwest 18th Street, Shenandoah, Miami, Miami-Dade County, Florida, 33145, United States</div>
        <div class="text gop precinct">Precinct Margin: R+2%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.39115945106603,-84.36127604199442&ll=39.39115945106603,-84.36127604199442&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/120.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=25.75585645,-80.21893209328482&ll=25.75585645,-80.21893209328482&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/121.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-123">Voter 123's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 123 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;55'53"N 77&deg;1'25"W</div>
        <div class="text address"><i>approx.</i> 3318, Georgia Avenue Northwest, Pleasant Plains, Washington, District of Columbia, 20012, United States</div>
        <div class="text dem precinct">Precinct Margin: D+90%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-124">Voter 124's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 124 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;50'10"N 116&deg;56'17"W</div>
        <div class="text address"><i>approx.</i> 8749, Almond Road, Winter Gardens, Lakeside, San Diego County, California, 92040, United States</div>
        <div class="text gop precinct">Precinct Margin: R+27%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.9314788,-77.02384157731024&ll=38.9314788,-77.02384157731024&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/122.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.836336,-116.938259&ll=32.836336,-116.938259&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/123.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-125">Voter 125's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 125 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;38'56"N 112&deg;17'27"W</div>
        <div class="text address"><i>approx.</i> Willowbrook Golf Course, 10600, North Boswell Boulevard, Sun City, Maricopa County, Arizona, 85373, United States</div>
        <div class="text gop precinct">Precinct Margin: R+23%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-126">Voter 126's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 126 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;27'5"N 86&deg;7'40"W</div>
        <div class="text address"><i>approx.</i> 3101, South Washington Street, Kokomo, Howard County, Indiana, 46902, United States</div>
        <div class="text gop precinct">Precinct Margin: R+23%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.64915805,-112.29095627493159&ll=33.64915805,-112.29095627493159&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/124.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.4515137,-86.1277831595088&ll=40.4515137,-86.1277831595088&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/125.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-127">Voter 127's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 127 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;1'29"N 88&deg;32'23"W</div>
        <div class="text address"><i>approx.</i> 205, West Irving Avenue, Middle Village, Oshkosh, Winnebago County, Wisconsin, 54901, United States</div>
        <div class="text dem precinct">Precinct Margin: D+25%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-128">Voter 128's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 128 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;23'25"N 122&deg;46'13"W</div>
        <div class="text address"><i>approx.</i> Tualatin Country Club, Tualatin River Trail, Cook Park, Tigard, Washington County, Oregon, 97062, United States</div>
        <div class="text dem precinct">Precinct Margin: D+35%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.0248365,-88.53986433333334&ll=44.0248365,-88.53986433333334&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/126.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.3903093,-122.77031482625532&ll=45.3903093,-122.77031482625532&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/127.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-129">Voter 129's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 129 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;13'46"N 121&deg;7'44"W</div>
        <div class="text address"><i>approx.</i> Metz Road, King City, Monterey County, California, 93930, United States</div>
        <div class="text dem precinct">Precinct Margin: D+37%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-130">Voter 130's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 130 voted for Trump</div> -->
        <div class="text coordinate"> 63&deg;25'2"N 157&deg;40'18"W</div>
        <div class="text address"><i>approx.</i> Unorganized Borough, Alaska, United States</div>
        <div class="text dem precinct">Precinct Margin: D+22%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.22957002819942,-121.12911091249975&ll=36.22957002819942,-121.12911091249975&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/128.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=63.417431050000005,-157.6718650484568&ll=63.417431050000005,-157.6718650484568&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/129.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-131">Voter 131's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 131 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;52'57"N 75&deg;21'39"W</div>
        <div class="text address"><i>approx.</i> 88, Medbury Road, Avendale, Nether Providence Township, Delaware County, Pennsylvania, 19086, United States</div>
        <div class="text dem precinct">Precinct Margin: D+28%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-132">Voter 132's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 132 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;25'20"N 87&deg;36'28"W</div>
        <div class="text address"><i>approx.</i> 1679, South State Street, Crete, Will County, Illinois, 60417, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.88255830532857,-75.3609345486896&ll=39.88255830532857,-75.3609345486896&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/130.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.4223922244898,-87.60795851020409&ll=41.4223922244898,-87.60795851020409&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/131.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-133">Voter 133's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 133 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;59'8"N 118&deg;5'33"W</div>
        <div class="text address"><i>approx.</i> 6530, Bequette Avenue, Pico Rivera, Los Angeles County, California, 90660, United States</div>
        <div class="text dem precinct">Precinct Margin: D+57%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-134">Voter 134's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 134 voted for Trump</div> -->
        <div class="text coordinate"> 21&deg;20'9"N 158&deg;4'26"W</div>
        <div class="text address"><i>approx.</i> Archery Center, Fort Barrette Road, City Center, Makakilo City, Kapolei, Honolulu County, Hawaii, 96707, United States</div>
        <div class="text dem precinct">Precinct Margin: D+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.98572371152788,-118.09254729938043&ll=33.98572371152788,-118.09254729938043&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/132.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=21.336065849999997,-158.07405933831978&ll=21.336065849999997,-158.07405933831978&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/133.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-135">Voter 135's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 135 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;48'30"N 84&deg;46'25"W</div>
        <div class="text address"><i>approx.</i> 676, Timber Ridge Trail, Paulding County, Georgia, 30141, United States</div>
        <div class="text dem precinct">Precinct Margin: D+1%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-136">Voter 136's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 136 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;33'9"N 84&deg;31'12"W</div>
        <div class="text address"><i>approx.</i> Grape Ruff Road, Scott County, Tennessee, 37841, United States</div>
        <div class="text gop precinct">Precinct Margin: R+76%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.80848058553242,-84.77365949327296&ll=33.80848058553242,-84.77365949327296&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/134.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.5525884,-84.5200912&ll=36.5525884,-84.5200912&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/135.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-137">Voter 137's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 137 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;56'37"N 118&deg;20'50"W</div>
        <div class="text address"><i>approx.</i> West 102nd Street, Inglewood, Los Angeles County, California, 90304, United States</div>
        <div class="text dem precinct">Precinct Margin: D+66%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-138">Voter 138's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 138 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;0'0"N 74&deg;21'5"W</div>
        <div class="text address"><i>approx.</i> 27, Western Avenue, Butler, Morris County, New Jersey, 07405, United States</div>
        <div class="text gop precinct">Precinct Margin: R+12%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.94362857660363,-118.3473745314416&ll=33.94362857660363,-118.3473745314416&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/136.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.000199790666464,-74.35163349481167&ll=41.000199790666464,-74.35163349481167&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/137.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-139">Voter 139's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 139 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;9'58"N 111&deg;56'10"W</div>
        <div class="text address"><i>approx.</i> 1600, Mohawk Lane, Ogden, Weber County, Utah, 84403, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-140">Voter 140's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 140 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;38'48"N 122&deg;39'26"W</div>
        <div class="text address"><i>approx.</i> 3505, N Street, Rose Village, Vancouver, Clark County, Washington, 98663, United States</div>
        <div class="text dem precinct">Precinct Margin: D+38%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.166247,-111.936289&ll=41.166247,-111.936289&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/138.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.646757,-122.6572305&ll=45.646757,-122.6572305&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/139.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-141">Voter 141's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 141 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;26'58"N 81&deg;41'45"W</div>
        <div class="text address"><i>approx.</i> 3888, West 20th Street, Brooklyn Centre, Cleveland, Cuyahoga County, Ohio, 44109, United States</div>
        <div class="text dem precinct">Precinct Margin: D+29%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-142">Voter 142's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 142 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;16'47"N 87&deg;48'41"W</div>
        <div class="text address"><i>approx.</i> 3425, East 11000N Road, Kankakee  County, Illinois, 60468, United States</div>
        <div class="text gop precinct">Precinct Margin: R+35%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.44960285185185,-81.69600581481481&ll=41.44960285185185,-81.69600581481481&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/140.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.2799900810959,-87.81164979448972&ll=41.2799900810959,-87.81164979448972&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/141.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-143">Voter 143's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 143 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;2'46"N 75&deg;4'35"W</div>
        <div class="text address"><i>approx.</i> 1260, Knorr Street, Lawndale, Philadelphia, Philadelphia County, Pennsylvania, 19149, United States</div>
        <div class="text dem precinct">Precinct Margin: D+63%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-144">Voter 144's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 144 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;45'31"N 86&deg;4'44"W</div>
        <div class="text address"><i>approx.</i> 50541, Little John Lane, Sherwood Forest Estates, Granger, Saint Joseph County, Indiana, 46530, United States</div>
        <div class="text gop precinct">Precinct Margin: R+10%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.04624604216423,-75.07648646047103&ll=40.04624604216423,-75.07648646047103&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/142.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.75863781879195,-86.0789837785235&ll=41.75863781879195,-86.0789837785235&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/143.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-145">Voter 145's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 145 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;58'22"N 121&deg;58'34"W</div>
        <div class="text address"><i>approx.</i> 2523, Harper Street, Yacht Harbor Manor, Twin Lakes, Santa Cruz County, California, 95062, United States</div>
        <div class="text dem precinct">Precinct Margin: D+62%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-146">Voter 146's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 146 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;46'47"N 115&deg;12'20"W</div>
        <div class="text address"><i>approx.</i> Butter Creek Road, Idaho County, Idaho, United States</div>
        <div class="text gop precinct">Precinct Margin: R+74%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.97282330897467,-121.97613225740513&ll=36.97282330897467,-121.97613225740513&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/144.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.77983132695823,-115.20575036563349&ll=45.77983132695823,-115.20575036563349&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/145.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-147">Voter 147's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 147 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;40'40"N 97&deg;22'15"W</div>
        <div class="text address"><i>approx.</i> 4733, Melita Avenue, Fort Worth, Tarrant County, Texas, 76133, United States</div>
        <div class="text dem precinct">Precinct Margin: D+14%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-148">Voter 148's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 148 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;27'56"N 87&deg;4'20"W</div>
        <div class="text address"><i>approx.</i> Factory Street, Valparaiso, Porter County, Indiana, 46385, United States</div>
        <div class="text gop precinct">Precinct Margin: R+3%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.678054599999996,-97.37085976264851&ll=32.678054599999996,-97.37085976264851&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/146.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.465590854842944,-87.07236999536129&ll=41.465590854842944,-87.07236999536129&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/147.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-149">Voter 149's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 149 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;46'43"N 122&deg;24'19"W</div>
        <div class="text address"><i>approx.</i> SOMA Pilipinas - Filipino Cultural Heritage District, 3rd Street, Union Square, San Francisco, California, 94107, United States</div>
        <div class="text dem precinct">Precinct Margin: D+80%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-150">Voter 150's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 150 voted for Trump</div> -->
        <div class="text coordinate"> 47&deg;40'48"N 117&deg;21'23"W</div>
        <div class="text address"><i>approx.</i> 2451, North Sycamore Street, Minnehaha, Spokane, Spokane County, Washington, 99217, United States</div>
        <div class="text gop precinct">Precinct Margin: R+16%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.7787098,-122.40540076661637&ll=37.7787098,-122.40540076661637&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/148.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.68004577777778,-117.35650646464647&ll=47.68004577777778,-117.35650646464647&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/149.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-151">Voter 151's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 151 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;30'34"N 90&deg;31'58"W</div>
        <div class="text address"><i>approx.</i> Great River Trail, Rock Island, Rock Island County, Illinois, 61299, United States</div>
        <div class="text dem precinct">Precinct Margin: D+43%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-152">Voter 152's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 152 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;24'8"N 83&deg;45'20"W</div>
        <div class="text address"><i>approx.</i> 8000, Park Road, Northfield Township, Washtenaw County, Michigan, 48189, United States</div>
        <div class="text dem precinct">Precinct Margin: D+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.50955070660479,-90.53279059637748&ll=41.50955070660479,-90.53279059637748&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/150.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.4024013,-83.7558087&ll=42.4024013,-83.7558087&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/151.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-153">Voter 153's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 153 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;57'10"N 92&deg;42'48"W</div>
        <div class="text address"><i>approx.</i> Hanley Road, Hudson, Saint Croix County, Wisconsin, 54016, United States</div>
        <div class="text gop precinct">Precinct Margin: R+0%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-154">Voter 154's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 154 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;43'53"N 121&deg;51'25"W</div>
        <div class="text address"><i>approx.</i> 948, West Sacramento Avenue, Chico, Butte County, California, 95926, United States</div>
        <div class="text dem precinct">Precinct Margin: D+62%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.952779817735816,-92.71335109073893&ll=44.952779817735816,-92.71335109073893&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/152.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.731647122448976,-121.85717587755103&ll=39.731647122448976,-121.85717587755103&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/153.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-155">Voter 155's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 155 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;0'51"N 117&deg;33'55"W</div>
        <div class="text address"><i>approx.</i> 3987, Bethany Way, Ontario, San Bernardino County, California, 91761, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-156">Voter 156's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 156 voted for Trump</div> -->
        <div class="text coordinate"> 27&deg;55'11"N 82&deg;47'7"W</div>
        <div class="text address"><i>approx.</i> Largo Senior High School, 2nd Avenue Northeast, Largo, Pinellas County, Florida, 33770, United States</div>
        <div class="text gop precinct">Precinct Margin: R+3%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.01417554149377,-117.56545301659828&ll=34.01417554149377,-117.56545301659828&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/154.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=27.91996365,-82.78552727626496&ll=27.91996365,-82.78552727626496&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/155.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-157">Voter 157's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 157 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;45'5"N 96&deg;42'57"W</div>
        <div class="text address"><i>approx.</i> Wilderness Park Hiker Trail, Lincoln, Lancaster County, Nebraska, 68512, United States</div>
        <div class="text dem precinct">Precinct Margin: D+4%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-158">Voter 158's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 158 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;38'54"N 73&deg;42'51"W</div>
        <div class="text address"><i>approx.</i> 287, Hungry Harbor Road, Village of Valley Stream, Town of Hempstead, Nassau County, New York, 11581, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.7515909,-96.7159121&ll=40.7515909,-96.7159121&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/156.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.6485426,-73.71429298808226&ll=40.6485426,-73.71429298808226&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/157.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-159">Voter 159's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 159 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;8'28"N 80&deg;46'0"W</div>
        <div class="text address"><i>approx.</i> Cottonwood Nature Trail, Olde Heritage, Charlotte, Mecklenburg County, North Carolina, 28270, United States</div>
        <div class="text dem precinct">Precinct Margin: D+33%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-160">Voter 160's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 160 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;46'23"N 87&deg;25'29"W</div>
        <div class="text address"><i>approx.</i> 9935, Pilgrim Trail, Escambia County, Florida, 32577, United States</div>
        <div class="text gop precinct">Precinct Margin: R+69%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.1413443,-80.766701&ll=35.1413443,-80.766701&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/158.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.773208,-87.424856&ll=30.773208,-87.424856&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/159.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-161">Voter 161's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 161 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;42'45"N 73&deg;50'51"W</div>
        <div class="text address"><i>approx.</i> 71-42, Kessel Street, Parkside, Queens, City of New York, New York, 11375, United States</div>
        <div class="text dem precinct">Precinct Margin: D+42%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-162">Voter 162's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 162 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;6'42"N 80&deg;54'59"W</div>
        <div class="text address"><i>approx.</i> 2061, Bermuda Hills Road, Richland County, South Carolina, 29223, United States</div>
        <div class="text dem precinct">Precinct Margin: D+28%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.7126133,-73.84775198910009&ll=40.7126133,-73.84775198910009&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/160.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.111874582829934,-80.91663049965354&ll=34.111874582829934,-80.91663049965354&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/161.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-163">Voter 163's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 163 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;50'37"N 76&deg;50'25"W</div>
        <div class="text address"><i>approx.</i> 3126, Squire Road, Westphalia Estates, Upper Marlboro, Prince George's County, Maryland, 20772, United States</div>
        <div class="text dem precinct">Precinct Margin: D+89%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-164">Voter 164's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 164 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;33'0"N 82&deg;56'31"W</div>
        <div class="text address"><i>approx.</i> Ridge Circle, Dublin, Laurens County, Georgia, 31021, United States</div>
        <div class="text gop precinct">Precinct Margin: R+25%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.843705549999996,-76.84046974609849&ll=38.843705549999996,-76.84046974609849&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/162.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.55011736592814,-82.94221659296696&ll=32.55011736592814,-82.94221659296696&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/163.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-165">Voter 165's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 165 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;22'52"N 111&deg;32'25"W</div>
        <div class="text address"><i>approx.</i> 546, East 37th Avenue, Apache Junction, Pinal County, Arizona, 85119, United States</div>
        <div class="text gop precinct">Precinct Margin: R+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-166">Voter 166's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 166 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;4'7"N 121&deg;17'59"W</div>
        <div class="text address"><i>approx.</i> 456, Northeast Seward Avenue, Bend, Deschutes County, Oregon, 97701, United States</div>
        <div class="text dem precinct">Precinct Margin: D+30%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.381285,-111.540502&ll=33.381285,-111.540502&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/164.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.068742,-121.29995571428572&ll=44.068742,-121.29995571428572&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/165.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-167">Voter 167's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 167 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;34'36"N 87&deg;19'3"W</div>
        <div class="text address"><i>approx.</i> 2232, Mississippi Street, Pulaski, Gary, Lake County, Indiana, 46407, United States</div>
        <div class="text dem precinct">Precinct Margin: D+92%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-168">Voter 168's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 168 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;24'38"N 111&deg;43'24"W</div>
        <div class="text address"><i>approx.</i> 498, East Boeing, Venture Out, Gilbert, Mesa, Maricopa County, Arizona, 85205, United States</div>
        <div class="text gop precinct">Precinct Margin: R+14%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.576881959612926,-87.31755723320347&ll=41.576881959612926,-87.31755723320347&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/166.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.4108097,-111.72348720204292&ll=33.4108097,-111.72348720204292&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/167.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-169">Voter 169's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 169 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;51'44"N 73&deg;55'46"W</div>
        <div class="text address"><i>approx.</i> 2, Sherman Avenue, Fort George, Manhattan, New York County, City of New York, New York, 10040, United States</div>
        <div class="text dem precinct">Precinct Margin: D+75%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-170">Voter 170's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 170 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;17'59"N 88&deg;24'14"W</div>
        <div class="text address"><i>approx.</i> Bentgrass Lane, Woodstock, McHenry County, Illinois, 60098, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.8624499,-73.9295059&ll=40.8624499,-73.9295059&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/168.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.2998444,-88.4039664&ll=42.2998444,-88.4039664&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/169.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-171">Voter 171's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 171 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;21'17"N 119&deg;0'59"W</div>
        <div class="text address"><i>approx.</i> 91, L Street, Civic Center, Bakersfield, Kern County, California, 93304, United States</div>
        <div class="text dem precinct">Precinct Margin: D+55%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-172">Voter 172's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 172 voted for Trump</div> -->
        <div class="text coordinate"> 28&deg;21'53"N 82&deg;41'17"W</div>
        <div class="text address"><i>approx.</i> 7644, Andrews Street, Club Wildwood, Hudson, Pasco County, Florida, 34667, United States</div>
        <div class="text gop precinct">Precinct Margin: R+30%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.35484569387755,-119.01663873469388&ll=35.35484569387755,-119.01663873469388&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/170.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=28.364889923076923,-82.688164&ll=28.364889923076923,-82.688164&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/171.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-173">Voter 173's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 173 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;47'16"N 96&deg;52'21"W</div>
        <div class="text address"><i>approx.</i> 3804, Delhi Street, Dallas, Dallas County, Texas, 75212, United States</div>
        <div class="text dem precinct">Precinct Margin: D+80%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-174">Voter 174's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 174 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;0'16"N 91&deg;26'13"W</div>
        <div class="text address"><i>approx.</i> 24258, Great River Road, Trempealeau, Trempealeau County, Wisconsin, 54661, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.7879463,-96.87269242427325&ll=32.7879463,-96.87269242427325&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/172.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.00450921739131,-91.43705295652174&ll=44.00450921739131,-91.43705295652174&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/173.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-175">Voter 175's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 175 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;28'31"N 91&deg;8'37"W</div>
        <div class="text address"><i>approx.</i> 2989, Addison Street, Belfair, Prescott Place, Baton Rouge, East Baton Rouge Parish, Louisiana, 70805, United States</div>
        <div class="text dem precinct">Precinct Margin: D+91%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-176">Voter 176's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 176 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;19'42"N 79&deg;44'13"W</div>
        <div class="text address"><i>approx.</i> Circleville School Site (Historic), Robbins Station Road, Cedar Glenn, Circleville, North Huntingdon Township, Westmoreland County, Pennsylvania, 15642, United States</div>
        <div class="text gop precinct">Precinct Margin: R+32%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.47543122312661,-91.14370833340473&ll=30.47543122312661,-91.14370833340473&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/174.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.3285564,-79.7370223&ll=40.3285564,-79.7370223&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/175.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-177">Voter 177's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 177 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;6'33"N 106&deg;31'4"W</div>
        <div class="text address"><i>approx.</i> Walmart Pharmacy, Paseo de las Montañas Trail, Casa Grande/South Glenwood Hills, Enchanted Park, Albuquerque, Bernalillo County, New Mexico, 87184, United States</div>
        <div class="text dem precinct">Precinct Margin: D+23%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-178">Voter 178's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 178 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;11'37"N 77&deg;12'23"W</div>
        <div class="text address"><i>approx.</i> 20617, Bell Bluff Road, Prathertown, Montgomery Village, Montgomery County, Maryland, 20879, United States</div>
        <div class="text dem precinct">Precinct Margin: D+56%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.1094403,-106.517839&ll=35.1094403,-106.517839&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/176.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.193715350000005,-77.20653415000001&ll=39.193715350000005,-77.20653415000001&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/177.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-179">Voter 179's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 179 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;34'14"N 112&deg;6'33"W</div>
        <div class="text address"><i>approx.</i> 9228, North 23rd Avenue, Phoenix, Maricopa County, Arizona, 85021, United States</div>
        <div class="text dem precinct">Precinct Margin: D+25%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-180">Voter 180's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 180 voted for Trump</div> -->
        <div class="text coordinate"> 26&deg;14'4"N 81&deg;40'47"W</div>
        <div class="text address"><i>approx.</i> 3472, 3rd Avenue Northwest, Golden Gate Estates, Collier County, Florida, 34120, United States</div>
        <div class="text gop precinct">Precinct Margin: R+34%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.570810699999996,-112.10921529947879&ll=33.570810699999996,-112.10921529947879&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/178.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.234456220194897,-81.67987766512466&ll=26.234456220194897,-81.67987766512466&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/179.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-181">Voter 181's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 181 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;15'18"N 74&deg;45'35"W</div>
        <div class="text address"><i>approx.</i> Mt Pleasant - Tuckahoe Road, Tuckahoe, Upper Township, Cape May County, New Jersey, 08270, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-182">Voter 182's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 182 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;46'40"N 95&deg;47'13"W</div>
        <div class="text address"><i>approx.</i> 23802, North Newport Bend, Harris County, Texas, 77494, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.25504932321404,-74.75982800810314&ll=39.25504932321404,-74.75982800810314&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/180.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.778052502397948,-95.78715138729643&ll=29.778052502397948,-95.78715138729643&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/181.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-183">Voter 183's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 183 voted for Biden</div> -->
        <div class="text coordinate"> 27&deg;55'3"N 82&deg;29'59"W</div>
        <div class="text address"><i>approx.</i> 3464, West San Pedro Street, Palma Ceia, Tampa, Hillsborough County, Florida, 33629, United States</div>
        <div class="text dem precinct">Precinct Margin: D+12%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-184">Voter 184's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 184 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;26'14"N 92&deg;19'2"W</div>
        <div class="text address"><i>approx.</i> 5391, Hess Road, Waterloo, Black Hawk County, Iowa, 50701, United States</div>
        <div class="text dem precinct">Precinct Margin: D+7%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=27.917586612244897,-82.49983904081633&ll=27.917586612244897,-82.49983904081633&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/182.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.437318708749814,-92.31742791790505&ll=42.437318708749814,-92.31742791790505&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/183.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-185">Voter 185's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 185 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;53'43"N 93&deg;21'35"W</div>
        <div class="text address"><i>approx.</i> 5318, West 60th Street, Edina, Hennepin County, Minnesota, 55436, United States</div>
        <div class="text dem precinct">Precinct Margin: D+35%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-186">Voter 186's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 186 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;47'46"N 83&deg;14'26"W</div>
        <div class="text address"><i>approx.</i> 998, Wildbrook Lane, Oakland County, Michigan, 48362, United States</div>
        <div class="text gop precinct">Precinct Margin: R+14%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.8952973,-93.3599129&ll=44.8952973,-93.3599129&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/184.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.796274,-83.240657&ll=42.796274,-83.240657&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/185.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-187">Voter 187's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 187 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;56'33"N 74&deg;4'40"W</div>
        <div class="text address"><i>approx.</i> The Ridgewood Country Club, 96, West Midland Avenue, Paramus, Bergen County, New Jersey, 07652, United States</div>
        <div class="text gop precinct">Precinct Margin: R+1%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-188">Voter 188's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 188 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;46'49"N 84&deg;2'3"W</div>
        <div class="text address"><i>approx.</i> 765, Hi View Lane, Blount County, Tennessee, 37801, United States</div>
        <div class="text gop precinct">Precinct Margin: R+49%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.94276895,-74.07796507650173&ll=40.94276895,-74.07796507650173&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/186.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.78032300364696,-84.03418011265374&ll=35.78032300364696,-84.03418011265374&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/187.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-189">Voter 189's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 189 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;43'17"N 93&deg;26'12"W</div>
        <div class="text address"><i>approx.</i> 4116, Grainwood Circle Northeast, Grainwood, Prior Lake, Scott County, Minnesota, 55372, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-190">Voter 190's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 190 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;28'44"N 122&deg;29'31"W</div>
        <div class="text address"><i>approx.</i> 5989, Southeast Jenne Lane, Pleasant Valley, Portland, Multnomah County, Oregon, 97236, United States</div>
        <div class="text dem precinct">Precinct Margin: D+18%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.72153270507523,-93.43680506296694&ll=44.72153270507523,-93.43680506296694&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/188.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.47897905,-122.49207534995136&ll=45.47897905,-122.49207534995136&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/189.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-191">Voter 191's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 191 voted for Biden</div> -->
        <div class="text coordinate"> 46&deg;50'16"N 96&deg;44'55"W</div>
        <div class="text address"><i>approx.</i> 36th Avenue South, Moorhead, Clay County, Minnesota, 56560, United States</div>
        <div class="text dem precinct">Precinct Margin: D+12%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-192">Voter 192's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 192 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;37'11"N 98&deg;31'43"W</div>
        <div class="text address"><i>approx.</i> 1678, Greystone Ridge, San Antonio, Bexar County, Texas, 78258, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=46.83793217165815,-96.74861823708737&ll=46.83793217165815,-96.74861823708737&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/190.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.6199170685966,-98.52878612487642&ll=29.6199170685966,-98.52878612487642&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/191.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-193">Voter 193's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 193 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;25'22"N 71&deg;4'14"W</div>
        <div class="text address"><i>approx.</i> 66;80, Charles Street, Malden Centre, Malden, Middlesex County, Massachusetts, 02148, United States</div>
        <div class="text dem precinct">Precinct Margin: D+65%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-194">Voter 194's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 194 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;56'40"N 82&deg;50'41"W</div>
        <div class="text address"><i>approx.</i> Yorktown Middle School, Red Apple Ridge Road, Columbus, Franklin County, Ohio, 43068-3983, United States</div>
        <div class="text dem precinct">Precinct Margin: D+57%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.42298455,-71.0706352014887&ll=42.42298455,-71.0706352014887&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/192.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.9445083,-82.8448984&ll=39.9445083,-82.8448984&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/193.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-195">Voter 195's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 195 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;42'47"N 90&deg;17'30"W</div>
        <div class="text address"><i>approx.</i> 7341, Wallington Walk, Norwood Court Apartments, Norwood Court, Saint Louis County, Missouri, 63121, United States</div>
        <div class="text dem precinct">Precinct Margin: D+89%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-196">Voter 196's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 196 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;24'26"N 96&deg;51'10"W</div>
        <div class="text address"><i>approx.</i> Sheaffer Full Life Center, Savell Circle, Waxahachie, Ellis County, Texas, 75165, United States</div>
        <div class="text gop precinct">Precinct Margin: R+37%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.713208127932035,-90.29167281701532&ll=38.713208127932035,-90.29167281701532&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/194.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.40735495,-96.85295660720841&ll=32.40735495,-96.85295660720841&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/195.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-197">Voter 197's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 197 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;44'16"N 74&deg;13'14"W</div>
        <div class="text address"><i>approx.</i> Munn Avenue, Irvington, Essex County, New Jersey, 07018:07052, United States</div>
        <div class="text dem precinct">Precinct Margin: D+90%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-198">Voter 198's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 198 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;41'56"N 77&deg;35'27"W</div>
        <div class="text address"><i>approx.</i> 10121, Leetonia Road, Tioga County, Pennsylvania, 16921, United States</div>
        <div class="text gop precinct">Precinct Margin: R+53%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.737829698632844,-74.22077956996141&ll=40.737829698632844,-74.22077956996141&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/196.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.69913510989348,-77.59105432252375&ll=41.69913510989348,-77.59105432252375&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/197.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-199">Voter 199's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 199 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;45'41"N 92&deg;25'16"W</div>
        <div class="text address"><i>approx.</i> 56, Perdido Circle, Saint Charles, Little Rock, Pulaski County, Arkansas, 72211, United States</div>
        <div class="text dem precinct">Precinct Margin: D+26%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-200">Voter 200's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 200 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;17'51"N 80&deg;20'27"W</div>
        <div class="text address"><i>approx.</i> 395, Hartgrove Road, Stokes County, North Carolina, 27021, United States</div>
        <div class="text gop precinct">Precinct Margin: R+54%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.76157938309493,-92.42129391664031&ll=34.76157938309493,-92.42129391664031&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/198.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.29771676188593,-80.34086939136594&ll=36.29771676188593,-80.34086939136594&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/199.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-201">Voter 201's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 201 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;38'57"N 105&deg;4'45"W</div>
        <div class="text address"><i>approx.</i> Comfort Inn & Suites, West Jefferson Avenue, Lakewood, Jefferson County, Colorado, 80235, United States</div>
        <div class="text dem precinct">Precinct Margin: D+25%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-202">Voter 202's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 202 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;8'27"N 83&deg;51'48"W</div>
        <div class="text address"><i>approx.</i> 7954, Lett Road, Knox County, Tennessee, 37721, United States</div>
        <div class="text gop precinct">Precinct Margin: R+60%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.64932425,-105.0794013322484&ll=39.64932425,-105.0794013322484&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/200.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.14108050567194,-83.86356432759504&ll=36.14108050567194,-83.86356432759504&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/201.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-203">Voter 203's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 203 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;6'39"N 82&deg;13'11"W</div>
        <div class="text address"><i>approx.</i> Southwest 61st Avenue, Marion County, Florida, 34476, United States</div>
        <div class="text gop precinct">Precinct Margin: R+21%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-204">Voter 204's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 204 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;23'16"N 79&deg;17'44"W</div>
        <div class="text address"><i>approx.</i> Rail 66 Country Trail, Roses, Jenks Township, Forest County, Pennsylvania, 16239, United States</div>
        <div class="text gop precinct">Precinct Margin: R+52%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.111036,-82.2199852&ll=29.111036,-82.2199852&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/202.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.388037141880325,-79.29568000861656&ll=41.388037141880325,-79.29568000861656&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/203.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-205">Voter 205's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 205 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;51'47"N 73&deg;49'15"W</div>
        <div class="text address"><i>approx.</i> 120, Elgar Place, Bronx County, The Bronx, City of New York, New York, 10475, United States</div>
        <div class="text dem precinct">Precinct Margin: D+84%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-206">Voter 206's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 206 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;3'20"N 106&deg;31'55"W</div>
        <div class="text address"><i>approx.</i> Analytical Solutions Incorporated, 10401, Research Road Southeast, Albuquerque, Bernalillo County, New Mexico, 87123, United States</div>
        <div class="text dem precinct">Precinct Margin: D+22%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.8632902,-73.82107950316464&ll=40.8632902,-73.82107950316464&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/204.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.0556906,-106.53197938500307&ll=35.0556906,-106.53197938500307&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/205.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-207">Voter 207's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 207 voted for Biden</div> -->
        <div class="text coordinate"> 43&deg;5'27"N 89&deg;21'31"W</div>
        <div class="text address"><i>approx.</i> 1875, East Main Street, Marquette, Madison, Dane County, Wisconsin, 53704, United States</div>
        <div class="text dem precinct">Precinct Margin: D+92%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-208">Voter 208's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 208 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;6'35"N 85&deg;39'32"W</div>
        <div class="text address"><i>approx.</i> 7954, Krupp Avenue Northeast, Plainfield Charter Township, Kent County, Michigan, 49321, United States</div>
        <div class="text gop precinct">Precinct Margin: R+13%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.09092769125997,-89.35865053557094&ll=43.09092769125997,-89.35865053557094&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/206.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.109960181606475,-85.65907365665547&ll=43.109960181606475,-85.65907365665547&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/207.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-209">Voter 209's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 209 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;39'7"N 84&deg;7'51"W</div>
        <div class="text address"><i>approx.</i> 449, Carters Grove Road, Oak Ridge, Centerville, Montgomery County, Ohio, 45459, United States</div>
        <div class="text dem precinct">Precinct Margin: D+10%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-210">Voter 210's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 210 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;24'39"N 72&deg;8'18"W</div>
        <div class="text address"><i>approx.</i> Miller Pond Road, Waterford, New London County, Connecticut, 06375, United States</div>
        <div class="text dem precinct">Precinct Margin: D+18%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.65197193682305,-84.13101033716224&ll=39.65197193682305,-84.13101033716224&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/208.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.41084935614711,-72.1385331989221&ll=41.41084935614711,-72.1385331989221&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/209.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-211">Voter 211's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 211 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;2'7"N 78&deg;16'31"W</div>
        <div class="text address"><i>approx.</i> 299, Capers Walk Lane, Sampson County, North Carolina, 28328, United States</div>
        <div class="text dem precinct">Precinct Margin: D+19%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-212">Voter 212's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 212 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;34'12"N 95&deg;8'39"W</div>
        <div class="text address"><i>approx.</i> Atchison Elementary School, 825, North 17th Street, Atchison, Atchison County, Kansas, 66002, United States</div>
        <div class="text gop precinct">Precinct Margin: R+17%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.035531,-78.27534&ll=35.035531,-78.27534&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/210.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.5700435,-95.14421075419847&ll=39.5700435,-95.14421075419847&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/211.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-213">Voter 213's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 213 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;36'42"N 83&deg;55'24"W</div>
        <div class="text address"><i>approx.</i> 752, North Barnard Street, Howell, Livingston County, Michigan, 48843, United States</div>
        <div class="text gop precinct">Precinct Margin: R+14%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-214">Voter 214's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 214 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;40'43"N 117&deg;14'47"W</div>
        <div class="text address"><i>approx.</i> Point Loma Wastewater Treatment Plant, 1902, Lands End Road, San Diego, San Diego County, California, 92106, United States</div>
        <div class="text dem precinct">Precinct Margin: D+23%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.61191344897959,-83.92356604081633&ll=42.61191344897959,-83.92356604081633&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/212.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.6787495,-117.24643125427963&ll=32.6787495,-117.24643125427963&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/213.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-215">Voter 215's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 215 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;33'47"N 74&deg;11'7"W</div>
        <div class="text address"><i>approx.</i> 83, Jamie Lane, Fresh Kills, Staten Island, City of New York, New York, 10312, United States</div>
        <div class="text gop precinct">Precinct Margin: R+11%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-216">Voter 216's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 216 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;25'49"N 79&deg;54'51"W</div>
        <div class="text address"><i>approx.</i> Riverview Trail, Squirrel Hill South, Pittsburgh, Allegheny County, Pennsylvania, 15217, United States</div>
        <div class="text dem precinct">Precinct Margin: D+63%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.56325715,-74.18552452043689&ll=40.56325715,-74.18552452043689&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/214.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.43033283252453,-79.91437086718881&ll=40.43033283252453,-79.91437086718881&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/215.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-217">Voter 217's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 217 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;44'15"N 117&deg;53'49"W</div>
        <div class="text address"><i>approx.</i> Monte Vista Elementary School, South Center Street, Santa Ana, Orange County, California, 92704, United States</div>
        <div class="text dem precinct">Precinct Margin: D+62%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-218">Voter 218's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 218 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;50'49"N 88&deg;53'5"W</div>
        <div class="text address"><i>approx.</i> 483, Green Way Lane, Country Club, Decatur, Macon County, Illinois, 62521, United States</div>
        <div class="text gop precinct">Precinct Margin: R+17%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.73753865,-117.89707284687879&ll=33.73753865,-117.89707284687879&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/216.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.847116511056036,-88.88495771658013&ll=39.847116511056036,-88.88495771658013&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/217.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-219">Voter 219's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 219 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;19'55"N 76&deg;28'9"W</div>
        <div class="text address"><i>approx.</i> 522, South Garfield Street, North Cornwall Township, Lebanon County, Pennsylvania, 17042, United States</div>
        <div class="text gop precinct">Precinct Margin: R+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-220">Voter 220's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 220 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;35'34"N 89&deg;22'2"W</div>
        <div class="text address"><i>approx.</i> 1675, North 900 East Road, Christian County, Illinois, 62568, United States</div>
        <div class="text gop precinct">Precinct Margin: R+41%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.3321219,-76.4692218&ll=40.3321219,-76.4692218&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/218.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.592953192369265,-89.36737820165379&ll=39.592953192369265,-89.36737820165379&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/219.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-221">Voter 221's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 221 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;38'7"N 89&deg;31'45"W</div>
        <div class="text address"><i>approx.</i> 6932, Beyer Road, Town of Crescent, Oneida County, Wisconsin, 54501, United States</div>
        <div class="text gop precinct">Precinct Margin: R+11%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-222">Voter 222's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 222 voted for Trump</div> -->
        <div class="text coordinate"> 27&deg;56'45"N 82&deg;15'12"W</div>
        <div class="text address"><i>approx.</i> 2077, Florida Street, Valrico, Brandon, Hillsborough County, Florida, 33594, United States</div>
        <div class="text gop precinct">Precinct Margin: R+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.63545244281509,-89.5292547194524&ll=45.63545244281509,-89.5292547194524&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/220.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=27.945891040816328,-82.25349732653062&ll=27.945891040816328,-82.25349732653062&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/221.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-223">Voter 223's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 223 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;5'32"N 117&deg;38'49"W</div>
        <div class="text address"><i>approx.</i> 8th Street, North Ontario, Upland, San Bernardino County, California, 91786, United States</div>
        <div class="text dem precinct">Precinct Margin: D+32%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-224">Voter 224's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 224 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;59'43"N 70&deg;31'36"W</div>
        <div class="text address"><i>approx.</i> 136, Leach Hill Road, Casco Village, Casco, Cumberland County, Maine, 04015, United States</div>
        <div class="text gop precinct">Precinct Margin: R+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.09248508107973,-117.64708981680579&ll=34.09248508107973,-117.64708981680579&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/222.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.9955295,-70.5268557&ll=43.9955295,-70.5268557&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/223.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-225">Voter 225's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 225 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;39'38"N 95&deg;40'45"W</div>
        <div class="text address"><i>approx.</i> 10592, Clodine Road, Sugar Land, Fort Bend County, Texas, 77469, United States</div>
        <div class="text dem precinct">Precinct Margin: D+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-226">Voter 226's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 226 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;41'30"N 111&deg;51'39"W</div>
        <div class="text address"><i>approx.</i> 3655, Christine Street, Millcreek, Salt Lake County, Utah, 84106, United States</div>
        <div class="text dem precinct">Precinct Margin: D+47%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.66066900016945,-95.67933258108876&ll=29.66066900016945,-95.67933258108876&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/224.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.69190342857143,-111.86100885714285&ll=40.69190342857143,-111.86100885714285&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/225.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-227">Voter 227's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 227 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;53'1"N 117&deg;51'3"W</div>
        <div class="text address"><i>approx.</i> 1256, Venice Avenue, Placentia, Orange County, California, 92870, United States</div>
        <div class="text gop precinct">Precinct Margin: R+6%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-228">Voter 228's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 228 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;59'27"N 85&deg;16'2"W</div>
        <div class="text address"><i>approx.</i> 3661, Craig Street, Milbro Heights, East Ridge, Hamilton County, Tennessee, 37412, United States</div>
        <div class="text gop precinct">Precinct Margin: R+14%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.883793749999995,-117.85106458897913&ll=33.883793749999995,-117.85106458897913&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/226.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.99083443074033,-85.2674425352735&ll=34.99083443074033,-85.2674425352735&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/227.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-229">Voter 229's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 229 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;17'55"N 111&deg;45'32"W</div>
        <div class="text address"><i>approx.</i> 1443, East Joseph Way, The Spectrum at Val Vista, Gilbert, Maricopa County, Arizona, 85295, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-230">Voter 230's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 230 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;11'55"N 85&deg;9'33"W</div>
        <div class="text address"><i>approx.</i> Food City, McConnell School Lane, Lakesite, Hamilton County, Tennessee, 37379, United States</div>
        <div class="text gop precinct">Precinct Margin: R+53%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.29883556879043,-111.75915834071782&ll=33.29883556879043,-111.75915834071782&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/228.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.19872775,-85.15921947643292&ll=35.19872775,-85.15921947643292&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/229.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-231">Voter 231's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 231 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;49'30"N 117&deg;47'12"W</div>
        <div class="text address"><i>approx.</i> 6363, East Blairwood Lane, Orange, Orange County, California, 92867, United States</div>
        <div class="text gop precinct">Precinct Margin: R+12%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-232">Voter 232's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 232 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;48'25"N 117&deg;50'44"W</div>
        <div class="text address"><i>approx.</i> 774, East Wilson Avenue, Orange, Orange County, California, 92867, United States</div>
        <div class="text dem precinct">Precinct Margin: D+5%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.825265508225485,-117.78687340006583&ll=33.825265508225485,-117.78687340006583&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/230.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.80719648796737,-117.8458011695548&ll=33.80719648796737,-117.8458011695548&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/231.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-233">Voter 233's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 233 voted for Biden</div> -->
        <div class="text coordinate"> 28&deg;8'51"N 81&deg;50'55"W</div>
        <div class="text address"><i>approx.</i> Florida Polytechnic University, 4700, Research Way, Lakeland, Polk County, Florida, 33805-8531, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-234">Voter 234's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 234 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;56'9"N 84&deg;6'7"W</div>
        <div class="text address"><i>approx.</i> 804, Chateaugay Road, Gulf Park-Belmont West-Plum Creek, Cedar Bluff, Knox County, Tennessee, 37923, United States</div>
        <div class="text gop precinct">Precinct Margin: R+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=28.147705350000003,-81.84878516048089&ll=28.147705350000003,-81.84878516048089&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/232.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.9360013071393,-84.10198996516453&ll=35.9360013071393,-84.10198996516453&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/233.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-235">Voter 235's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 235 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;20'12"N 99&deg;34'45"W</div>
        <div class="text address"><i>approx.</i> 6950, County Road 223, Callahan County, Texas, 79510, United States</div>
        <div class="text gop precinct">Precinct Margin: R+77%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-236">Voter 236's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 236 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;17'37"N 85&deg;57'20"W</div>
        <div class="text address"><i>approx.</i> Georgetown Lake, Baylor Wissman Road, Georgetown, Floyd County, Indiana, 47122, United States</div>
        <div class="text gop precinct">Precinct Margin: R+21%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.33691592593688,-99.57929020413046&ll=32.33691592593688,-99.57929020413046&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/234.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.2936784,-85.9558012&ll=38.2936784,-85.9558012&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/235.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-237">Voter 237's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 237 voted for Biden</div> -->
        <div class="text coordinate"> 61&deg;11'36"N 149&deg;51'3"W</div>
        <div class="text address"><i>approx.</i> 2984, Madison Way, College Village, Anchorage, Alaska, 99508, Anchorage</div>
        <div class="text dem precinct">Precinct Margin: D+25%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-238">Voter 238's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 238 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;38'55"N 104&deg;44'24"W</div>
        <div class="text address"><i>approx.</i> 3601, S Nepal Street, Aurora, Arapahoe County, Colorado, 80013, United States</div>
        <div class="text dem precinct">Precinct Margin: D+18%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=61.193345794750066,-149.85084016688867&ll=61.193345794750066,-149.85084016688867&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/236.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.648888,-104.740235&ll=39.648888,-104.740235&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/237.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-239">Voter 239's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 239 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;23'47"N 121&deg;26'42"W</div>
        <div class="text address"><i>approx.</i> 5080, Orchid Ranch Way, Elk Grove, Sacramento County, California, 95757, United States</div>
        <div class="text dem precinct">Precinct Margin: D+36%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-240">Voter 240's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 240 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;55'37"N 121&deg;45'7"W</div>
        <div class="text address"><i>approx.</i> T S MacQuiddy Elementary School, Martinelli Street, Watsonville, Santa Cruz County, California, 95076, United States</div>
        <div class="text dem precinct">Precinct Margin: D+54%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.396451440164164,-121.4450535153437&ll=38.396451440164164,-121.4450535153437&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/238.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.9271888,-121.7519863&ll=36.9271888,-121.7519863&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/239.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-241">Voter 241's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 241 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;35'13"N 84&deg;37'12"W</div>
        <div class="text address"><i>approx.</i> Nathaniel Lane, Fulton County, Georgia, 30213, United States</div>
        <div class="text dem precinct">Precinct Margin: D+91%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-242">Voter 242's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 242 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;45'57"N 96&deg;41'41"W</div>
        <div class="text address"><i>approx.</i> 1899, Center Park Road, Lincoln, Lancaster County, Nebraska, 68512, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.587060463857675,-84.62001571551966&ll=33.587060463857675,-84.62001571551966&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/240.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.766004,-96.6947501&ll=40.766004,-96.6947501&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/241.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-243">Voter 243's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 243 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;47'34"N 90&deg;41'41"W</div>
        <div class="text address"><i>approx.</i> Dude's Way, O’Fallon, Saint Charles County, Missouri, 63366, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-244">Voter 244's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 244 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;44'28"N 96&deg;46'32"W</div>
        <div class="text address"><i>approx.</i> 1020, Sargent Road, Sargent, Dallas, Dallas County, Texas, 75203, United States</div>
        <div class="text dem precinct">Precinct Margin: D+85%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.792946440322766,-90.69481534390705&ll=38.792946440322766,-90.69481534390705&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/242.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.741212250000004,-96.7757376&ll=32.741212250000004,-96.7757376&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/243.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-245">Voter 245's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 245 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;36'27"N 117&deg;11'29"W</div>
        <div class="text address"><i>approx.</i> 26761, Chamomile Street, Greer Ranch, Murrieta, Riverside County, California, 92562, United States</div>
        <div class="text gop precinct">Precinct Margin: R+22%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-246">Voter 246's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 246 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;25'46"N 119&deg;1'38"W</div>
        <div class="text address"><i>approx.</i> North High School, Galaxy Avenue, Kern County, California, 93301, United States</div>
        <div class="text gop precinct">Precinct Margin: R+53%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.607659554627936,-117.19162051661016&ll=33.607659554627936,-117.19162051661016&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/244.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.4295962,-119.02734580029981&ll=35.4295962,-119.02734580029981&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/245.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-247">Voter 247's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 247 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;2'0"N 75&deg;4'30"W</div>
        <div class="text address"><i>approx.</i> 1901, Lardner Street, Philadelphia, Philadelphia County, Pennsylvania, 19149, United States</div>
        <div class="text dem precinct">Precinct Margin: D+55%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-248">Voter 248's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 248 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;40'32"N 72&deg;20'2"W</div>
        <div class="text address"><i>approx.</i> 26, Hunt Road, Columbia, Tolland County, Connecticut, 06237, United States</div>
        <div class="text dem precinct">Precinct Margin: D+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.033573,-75.075095&ll=40.033573,-75.075095&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/246.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.6756005,-72.3339662&ll=41.6756005,-72.3339662&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/247.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-249">Voter 249's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 249 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;16'35"N 72&deg;36'5"W</div>
        <div class="text address"><i>approx.</i> Island Avenue Elementary School, 20, Island Avenue, Madison, New Haven County, Connecticut, 06443, United States</div>
        <div class="text dem precinct">Precinct Margin: D+27%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-250">Voter 250's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 250 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;43'45"N 83&deg;38'45"W</div>
        <div class="text address"><i>approx.</i> Pelton Lane, Bedford Township, Monroe County, Michigan, 49267, United States</div>
        <div class="text gop precinct">Precinct Margin: R+12%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.2766382,-72.60155775734555&ll=41.2766382,-72.60155775734555&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/248.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.7293976816345,-83.64587918253463&ll=41.7293976816345,-83.64587918253463&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/249.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-251">Voter 251's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 251 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;22'30"N 71&deg;3'47"W</div>
        <div class="text address"><i>approx.</i> 8, Cordis Street, Charlestown, Boston, Suffolk County, Massachusetts, 02129, United States</div>
        <div class="text dem precinct">Precinct Margin: D+65%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-252">Voter 252's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 252 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;38'43"N 86&deg;28'20"W</div>
        <div class="text address"><i>approx.</i> Hays Preserve Trail, Huntsville, Madison County, Alabama, 35763, United States</div>
        <div class="text gop precinct">Precinct Margin: R+40%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.3750925,-71.06309446940585&ll=42.3750925,-71.06309446940585&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/250.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.645473198518474,-86.47222462527283&ll=34.645473198518474,-86.47222462527283&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/251.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-253">Voter 253's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 253 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;2'34"N 86&deg;39'42"W</div>
        <div class="text address"><i>approx.</i> 5333, Cane Ridge Road, Bell Hollow Apartments, Nashville-Davidson, Davidson County, Tennessee, 37013, United States</div>
        <div class="text dem precinct">Precinct Margin: D+55%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-254">Voter 254's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 254 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;36'13"N 119&deg;47'13"W</div>
        <div class="text address"><i>approx.</i> 291, Harmony Lane, Sun Valley, Washoe County, Nevada, 89433, United States</div>
        <div class="text dem precinct">Precinct Margin: D+15%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.04280279801514,-86.66174504847902&ll=36.04280279801514,-86.66174504847902&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/252.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.603666777777775,-119.78696922222221&ll=39.603666777777775,-119.78696922222221&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/253.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-255">Voter 255's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 255 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;39'25"N 71&deg;19'56"W</div>
        <div class="text address"><i>approx.</i> 274;276, University Avenue, Pawtucketville, Lowell, Middlesex County, Massachusetts, 01854-5141, United States</div>
        <div class="text dem precinct">Precinct Margin: D+33%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-256">Voter 256's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 256 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;21'9"N 76&deg;8'21"W</div>
        <div class="text address"><i>approx.</i> 228, West Oak Avenue, Robesonia, Berks County, Pennsylvania, 19551, United States</div>
        <div class="text gop precinct">Precinct Margin: R+26%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.657007300000004,-71.33234395334387&ll=42.657007300000004,-71.33234395334387&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/254.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.35257357142857,-76.13927042857142&ll=40.35257357142857,-76.13927042857142&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/255.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-257">Voter 257's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 257 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;34'56"N 73&deg;49'1"W</div>
        <div class="text address"><i>approx.</i> 79, Crescent Creek Way, Dutch Mills, Town of Bethlehem, Albany County, New York, 12158, United States</div>
        <div class="text dem precinct">Precinct Margin: D+36%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-258">Voter 258's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 258 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;14'50"N 80&deg;9'51"W</div>
        <div class="text address"><i>approx.</i> 117, Fulton Road, Hill Church, North Strabane Township, Washington County, Pennsylvania, 15317, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.5823917,-73.8169715&ll=42.5823917,-73.8169715&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/256.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.247362439820556,-80.16430975349365&ll=40.247362439820556,-80.16430975349365&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/257.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-259">Voter 259's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 259 voted for Biden</div> -->
        <div class="text coordinate"> 25&deg;46'46"N 80&deg;20'46"W</div>
        <div class="text address"><i>approx.</i> 1016, Northwest 87th Avenue, Miami-Dade County, Florida, 33172, United States</div>
        <div class="text gop precinct">Precinct Margin: R+3%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-260">Voter 260's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 260 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;54'42"N 87&deg;52'27"W</div>
        <div class="text address"><i>approx.</i> 1640, Monroe Avenue, South Milwaukee, Milwaukee County, Wisconsin, 53172, United States</div>
        <div class="text dem precinct">Precinct Margin: D+0%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=25.7796406,-80.3463239&ll=25.7796406,-80.3463239&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/258.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.911925279256515,-87.87444412257891&ll=42.911925279256515,-87.87444412257891&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/259.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-261">Voter 261's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 261 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;4'31"N 115&deg;18'11"W</div>
        <div class="text address"><i>approx.</i> 9714, Hawk Cliff Avenue, Spinnaker at Southwest Ranch, Spring Valley, Clark County, Nevada, 89148, United States</div>
        <div class="text dem precinct">Precinct Margin: D+23%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-262">Voter 262's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 262 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;36'42"N 83&deg;20'44"W</div>
        <div class="text address"><i>approx.</i> 1849, Cass Lake Front Street, Keego Harbor, Oakland County, Michigan, 48320, United States</div>
        <div class="text dem precinct">Precinct Margin: D+5%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.07549980341387,-115.30308018891746&ll=36.07549980341387,-115.30308018891746&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/260.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.61194301963703,-83.34578761312723&ll=42.61194301963703,-83.34578761312723&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/261.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-263">Voter 263's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 263 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;58'35"N 123&deg;4'25"W</div>
        <div class="text address"><i>approx.</i> 2943, Wallace Road Northwest, Salemtowne, Salem, Polk County, Oregon, 97304, United States</div>
        <div class="text dem precinct">Precinct Margin: D+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-264">Voter 264's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 264 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;58'50"N 122&deg;49'36"W</div>
        <div class="text address"><i>approx.</i> 11417, Evergreen Road Northeast, Marion County, Oregon, 97381, United States</div>
        <div class="text gop precinct">Precinct Margin: R+31%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.97662868275546,-123.07365135895054&ll=44.97662868275546,-123.07365135895054&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/262.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.980680562057714,-122.82687751538268&ll=44.980680562057714,-122.82687751538268&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/263.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-265">Voter 265's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 265 voted for Biden</div> -->
        <div class="text coordinate"> 46&deg;43'13"N 92&deg;27'26"W</div>
        <div class="text address"><i>approx.</i> 34, 6th Street, Cloquet, Carlton County, Minnesota, 55720, United States</div>
        <div class="text dem precinct">Precinct Margin: D+11%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-266">Voter 266's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 266 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;52'20"N 117&deg;44'46"W</div>
        <div class="text address"><i>approx.</i> 4746, Tycana Road, Humboldt County, Nevada, 89445, United States</div>
        <div class="text gop precinct">Precinct Margin: R+71%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=46.72032281642889,-92.45747361661115&ll=46.72032281642889,-92.45747361661115&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/264.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.87226681495458,-117.74636819503193&ll=40.87226681495458,-117.74636819503193&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/265.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-267">Voter 267's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 267 voted for Biden</div> -->
        <div class="text coordinate"> 27&deg;56'1"N 82&deg;28'30"W</div>
        <div class="text address"><i>approx.</i> 1539, West Morrison Avenue, Historic Hyde Park North Neighborhood, Hyde Park, Tampa, Hillsborough County, Florida, 33606, United States</div>
        <div class="text dem precinct">Precinct Margin: D+14%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-268">Voter 268's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 268 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;59'54"N 91&deg;43'39"W</div>
        <div class="text address"><i>approx.</i> Colfax Pharmacy, River Street, Colfax, Dunn County, Wisconsin, 54730, United States</div>
        <div class="text gop precinct">Precinct Margin: R+5%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=27.933765551020407,-82.47524510204082&ll=27.933765551020407,-82.47524510204082&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/266.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.9984992,-91.7275112&ll=44.9984992,-91.7275112&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/267.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-269">Voter 269's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 269 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;34'58"N 90&deg;22'4"W</div>
        <div class="text address"><i>approx.</i> 422, Somerset Avenue, Sherwood Forest, Webster Groves, Saint Louis County, Missouri, 63119, United States</div>
        <div class="text dem precinct">Precinct Margin: D+43%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-270">Voter 270's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 270 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;8'42"N 82&deg;58'58"W</div>
        <div class="text address"><i>approx.</i> Hughes Road, Elbert County, Georgia, 30634, United States</div>
        <div class="text gop precinct">Precinct Margin: R+26%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.5828509019902,-90.36780904790481&ll=38.5828509019902,-90.36780904790481&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/268.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.14504223770826,-82.9828312917132&ll=34.14504223770826,-82.9828312917132&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/269.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-271">Voter 271's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 271 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;23'30"N 76&deg;34'2"W</div>
        <div class="text address"><i>approx.</i> 1664, Thetford Road, Knettishall, Towson, Baltimore County, Maryland, 21286, United States</div>
        <div class="text dem precinct">Precinct Margin: D+51%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-272">Voter 272's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 272 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;42'40"N 79&deg;50'14"W</div>
        <div class="text address"><i>approx.</i> 35284, Armstrong Road, Steuben Township, Crawford County, Pennsylvania, 16360, United States</div>
        <div class="text gop precinct">Precinct Margin: R+51%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.391846,-76.5673196&ll=39.391846,-76.5673196&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/270.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.711236917726026,-79.83736753208707&ll=41.711236917726026,-79.83736753208707&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/271.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-273">Voter 273's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 273 voted for Biden</div> -->
        <div class="text coordinate"> 43&deg;3'6"N 76&deg;5'51"W</div>
        <div class="text address"><i>approx.</i> 636, Hazelwood Avenue, Salt Springs, City of Syracuse, Syracuse, Onondaga County, New York, 13224, United States</div>
        <div class="text dem precinct">Precinct Margin: D+82%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-274">Voter 274's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 274 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;52'21"N 87&deg;59'40"W</div>
        <div class="text address"><i>approx.</i> 6482, West Ryan Road, Franklin, Milwaukee County, Wisconsin, 53132, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.0517034,-76.09763515&ll=43.0517034,-76.09763515&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/272.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.872751969671846,-87.99458408590678&ll=42.872751969671846,-87.99458408590678&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/273.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-275">Voter 275's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 275 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;47'30"N 71&deg;6'10"W</div>
        <div class="text address"><i>approx.</i> 30, Bennington Street, Haverhill, Essex County, Massachusetts, 01832, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-276">Voter 276's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 276 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;17'53"N 119&deg;7'25"W</div>
        <div class="text address"><i>approx.</i> 6299, Long Valley Way, Bakersfield, Kern County, California, 93311, United States</div>
        <div class="text dem precinct">Precinct Margin: D+7%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.791692499999996,-71.10292753125084&ll=42.791692499999996,-71.10292753125084&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/274.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.298111,-119.123886&ll=35.298111,-119.123886&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/275.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-277">Voter 277's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 277 voted for Biden</div> -->
        <div class="text coordinate"> 25&deg;57'46"N 97&deg;34'14"W</div>
        <div class="text address"><i>approx.</i> Carmen Avenue, Brownsville, Cameron County, Texas, United States</div>
        <div class="text dem precinct">Precinct Margin: D+18%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-278">Voter 278's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 278 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;41'40"N 86&deg;16'6"W</div>
        <div class="text address"><i>approx.</i> 129, Turkey Ridge Road, St. Clair County, Alabama, 35131, United States</div>
        <div class="text gop precinct">Precinct Margin: R+81%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=25.962806890234457,-97.57079959487282&ll=25.962806890234457,-97.57079959487282&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/276.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.69456200867762,-86.26834352555633&ll=33.69456200867762,-86.26834352555633&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/277.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-279">Voter 279's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 279 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;59'12"N 92&deg;58'28"W</div>
        <div class="text address"><i>approx.</i> 2619, Granada Avenue North, Oakdale, Washington County, Minnesota, 55128, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-280">Voter 280's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 280 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;32'6"N 77&deg;17'44"W</div>
        <div class="text address"><i>approx.</i> 1729, Casey Meadows Terrace, Forest Meadow Mobile Home Park, Henrico County, Virginia, 23150, United States</div>
        <div class="text dem precinct">Precinct Margin: D+38%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.98687045454545,-92.97458990909091&ll=44.98687045454545,-92.97458990909091&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/278.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.53522285,-77.29574172291673&ll=37.53522285,-77.29574172291673&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/279.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-281">Voter 281's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 281 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;24'54"N 77&deg;47'57"W</div>
        <div class="text address"><i>approx.</i> 6211, Clayville Lane, Chesterfield County, Virginia, 23120, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-282">Voter 282's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 282 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;14'1"N 86&deg;11'49"W</div>
        <div class="text address"><i>approx.</i> Popeyes, Gordon Street, Shady Park Neighbourhood, Muskegon Township, Muskegon County, Michigan, 49442, United States</div>
        <div class="text dem precinct">Precinct Margin: D+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.4152702,-77.79944&ll=37.4152702,-77.79944&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/280.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.2338625,-86.1969789&ll=43.2338625,-86.1969789&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/281.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-283">Voter 283's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 283 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;27'25"N 83&deg;12'31"W</div>
        <div class="text address"><i>approx.</i> 16225, Roanoke Street, Southfield, Oakland County, Michigan, 48075, United States</div>
        <div class="text dem precinct">Precinct Margin: D+89%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-284">Voter 284's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 284 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;57'45"N 90&deg;15'33"W</div>
        <div class="text address"><i>approx.</i> Moss Lane, Jefferson Parish, Louisiana, 70094, United States</div>
        <div class="text dem precinct">Precinct Margin: D+23%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.457205,-83.208675&ll=42.457205,-83.208675&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/282.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.962768,-90.259437&ll=29.962768,-90.259437&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/283.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-285">Voter 285's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 285 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;47'3"N 104&deg;50'47"W</div>
        <div class="text address"><i>approx.</i> 12150, East 48th Avenue, Montbello, Denver, Colorado, 80239, United States</div>
        <div class="text dem precinct">Precinct Margin: D+77%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-286">Voter 286's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 286 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;50'37"N 80&deg;35'2"W</div>
        <div class="text address"><i>approx.</i> 129, Grave Creek Road, Loudenville, Marshall County, West Virginia, 26033, United States</div>
        <div class="text gop precinct">Precinct Margin: R+83%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.784380299999995,-104.8465218495529&ll=39.784380299999995,-104.8465218495529&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/284.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.84384535287693,-80.58411853084876&ll=39.84384535287693,-80.58411853084876&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/285.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-287">Voter 287's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 287 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;16'36"N 118&deg;40'41"W</div>
        <div class="text address"><i>approx.</i> 5808, East Malton Avenue, White Oak, Simi Valley, Ventura County, California, 93063, United States</div>
        <div class="text dem precinct">Precinct Margin: D+5%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-288">Voter 288's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 288 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;20'52"N 103&deg;46'51"W</div>
        <div class="text address"><i>approx.</i> 656, Hearst Avenue, Lead, Lawrence County, South Dakota, 57754, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.276941,-118.67819265&ll=34.276941,-118.67819265&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/286.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.347968868563285,-103.78100095750786&ll=44.347968868563285,-103.78100095750786&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/287.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-289">Voter 289's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 289 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;47'49"N 84&deg;11'56"W</div>
        <div class="text address"><i>approx.</i> 4614, Hairston Crossing Place, DeKalb County, Georgia, 30083, United States</div>
        <div class="text dem precinct">Precinct Margin: D+82%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-290">Voter 290's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 290 voted for Trump</div> -->
        <div class="text coordinate"> 26&deg;1'46"N 80&deg;9'47"W</div>
        <div class="text address"><i>approx.</i> 2700, Scott Street, Hollywood, Broward County, Florida, 33020, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.79699799205405,-84.19899272077221&ll=33.79699799205405,-84.19899272077221&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/288.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.029479,-80.163128&ll=26.029479,-80.163128&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/289.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-291">Voter 291's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 291 voted for Biden</div> -->
        <div class="text coordinate"> 28&deg;41'26"N 81&deg;36'33"W</div>
        <div class="text address"><i>approx.</i> Lake Apopka Wildlife Drive (Laughlin Road), Orange County, Florida, 34778, United States</div>
        <div class="text dem precinct">Precinct Margin: D+29%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-292">Voter 292's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 292 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;44'4"N 73&deg;6'47"W</div>
        <div class="text address"><i>approx.</i> 103, Matthews Road, West Sayville, Suffolk County, New York, 11769, United States</div>
        <div class="text gop precinct">Precinct Margin: R+16%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=28.69063306508175,-81.60927416406825&ll=28.69063306508175,-81.60927416406825&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/290.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.73456205,-73.11316277170286&ll=40.73456205,-73.11316277170286&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/291.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-293">Voter 293's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 293 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;34'36"N 90&deg;55'11"W</div>
        <div class="text address"><i>approx.</i> 10354, D Taylor Street, Livingston Parish, Louisiana, 70706, United States</div>
        <div class="text gop precinct">Precinct Margin: R+75%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-294">Voter 294's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 294 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;6'43"N 84&deg;27'44"W</div>
        <div class="text address"><i>approx.</i> 1606, Barnes Road, Woodstock, Cherokee County, Georgia, 30188, United States</div>
        <div class="text gop precinct">Precinct Margin: R+23%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.576888,-90.91990741935511&ll=30.576888,-90.91990741935511&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/292.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.11202495863785,-84.46223183099686&ll=34.11202495863785,-84.46223183099686&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/293.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-295">Voter 295's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 295 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;37'3"N 74&deg;24'45"W</div>
        <div class="text address"><i>approx.</i> 318, Franklin Place, Plainfield, Union County, New Jersey, 07060, United States</div>
        <div class="text dem precinct">Precinct Margin: D+74%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-296">Voter 296's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 296 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;26'41"N 99&deg;22'10"W</div>
        <div class="text address"><i>approx.</i> 1076, East Avenue, Westside Plaza Mobile Home Park, Holdrege, Phelps County, Nebraska, 68949, United States</div>
        <div class="text gop precinct">Precinct Margin: R+55%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.617727477686515,-74.41258744800479&ll=40.617727477686515,-74.41258744800479&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/294.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.44492357142857,-99.36971622448979&ll=40.44492357142857,-99.36971622448979&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/295.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-297">Voter 297's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 297 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;6'36"N 80&deg;7'45"W</div>
        <div class="text address"><i>approx.</i> 1032, Southeast 13th Terrace, Lauderdale Harbors, Fort Lauderdale, Broward County, Florida, 33316, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-298">Voter 298's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 298 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;55'55"N 94&deg;11'6"W</div>
        <div class="text address"><i>approx.</i> 25, Riverwood Avenue, West Fork, Washington County, Arkansas, 72774, United States</div>
        <div class="text gop precinct">Precinct Margin: R+26%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.11004017560521,-80.12917477648735&ll=26.11004017560521,-80.12917477648735&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/296.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.9320691,-94.1852389&ll=35.9320691,-94.1852389&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/297.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-299">Voter 299's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 299 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;50'31"N 73&deg;59'3"W</div>
        <div class="text address"><i>approx.</i> 1235, 15th Street, Fort Lee, Bergen County, New Jersey, 07024, United States</div>
        <div class="text dem precinct">Precinct Margin: D+26%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-300">Voter 300's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 300 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;25'56"N 77&deg;38'46"W</div>
        <div class="text address"><i>approx.</i> 250, Smith Church Road, Roanoke Rapids, Halifax County, North Carolina, 27870, United States</div>
        <div class="text gop precinct">Precinct Margin: R+16%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.84205395833333,-73.984318375&ll=40.84205395833333,-73.984318375&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/298.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.4324228,-77.64626591830444&ll=36.4324228,-77.64626591830444&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/299.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-301">Voter 301's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 301 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;35'52"N 73&deg;49'54"W</div>
        <div class="text address"><i>approx.</i> 801, Feura Bush Road, Houcks Corners, Town of Bethlehem, Albany County, New York, 12054, United States</div>
        <div class="text dem precinct">Precinct Margin: D+40%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-302">Voter 302's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 302 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;46'45"N 97&deg;3'24"W</div>
        <div class="text address"><i>approx.</i> 404, Cottonwood Avenue, Milford, Seward County, Nebraska, 68405, United States</div>
        <div class="text gop precinct">Precinct Margin: R+47%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.5980334,-73.8317661&ll=42.5980334,-73.8317661&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/300.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.779412397541286,-97.05666971733292&ll=40.779412397541286,-97.05666971733292&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/301.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-303">Voter 303's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 303 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;13'20"N 98&deg;5'54"W</div>
        <div class="text address"><i>approx.</i> 217, East Nolana Loop, Arriaga Colonia, Hidalgo County, Texas, 78516, United States</div>
        <div class="text dem precinct">Precinct Margin: D+31%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-304">Voter 304's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 304 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;7'30"N 115&deg;16'18"W</div>
        <div class="text address"><i>approx.</i> Desert Breeze Community Center, West Spring Mountain Road, Spring Valley, Clark County, Nevada, 89147, United States</div>
        <div class="text dem precinct">Precinct Margin: D+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.222245233539383,-98.09859639937348&ll=26.222245233539383,-98.09859639937348&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/302.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.1252,-115.2718&ll=36.1252,-115.2718&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/303.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-305">Voter 305's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 305 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;1'22"N 77&deg;2'22"W</div>
        <div class="text address"><i>approx.</i> 10323, Green Holly Terrace, Northbrook Estates, Forest Estates, Wheaton, Montgomery County, Maryland, 20902, United States</div>
        <div class="text dem precinct">Precinct Margin: D+73%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-306">Voter 306's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 306 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;47'34"N 87&deg;39'31"W</div>
        <div class="text address"><i>approx.</i> 5528, South Ada Street, West Englewood, Chicago, Lake Township, Cook County, Illinois, 60636, United States</div>
        <div class="text dem precinct">Precinct Margin: D+91%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.02300774432179,-77.03951098234259&ll=39.02300774432179,-77.03951098234259&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/304.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.79283355,-87.65875887168289&ll=41.79283355,-87.65875887168289&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/305.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-307">Voter 307's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 307 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;46'57"N 73&deg;10'34"W</div>
        <div class="text address"><i>approx.</i> 94, Sportsmen Street, Suffolk County, New York, 11722, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-308">Voter 308's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 308 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;8'31"N 81&deg;46'29"W</div>
        <div class="text address"><i>approx.</i> 4832, Boneta Road, Granger Township, Medina County, Ohio, 44256, United States</div>
        <div class="text gop precinct">Precinct Margin: R+37%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.78267575,-73.17635330312484&ll=40.78267575,-73.17635330312484&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/306.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.142196733998,-81.7748324&ll=41.142196733998,-81.7748324&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/307.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-309">Voter 309's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 309 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;3'50"N 80&deg;59'1"W</div>
        <div class="text address"><i>approx.</i> Sunglow Lane, Richland County, South Carolina, 29223, United States</div>
        <div class="text dem precinct">Precinct Margin: D+83%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-310">Voter 310's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 310 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;23'55"N 91&deg;22'15"W</div>
        <div class="text address"><i>approx.</i> Keokuk Power Plant, Orleans Avenue, Keokuk, Lee County, Iowa, 52632, United States</div>
        <div class="text gop precinct">Precinct Margin: R+25%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.064018250000004,-80.98380695705742&ll=34.064018250000004,-80.98380695705742&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/308.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.398822499999994,-91.37089568851687&ll=40.398822499999994,-91.37089568851687&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/309.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-311">Voter 311's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 311 voted for Biden</div> -->
        <div class="text coordinate"> 47&deg;39'20"N 122&deg;21'29"W</div>
        <div class="text address"><i>approx.</i> 120, Northwest 40th Street, Fremont, Seattle, King County, Washington, 98107, United States</div>
        <div class="text dem precinct">Precinct Margin: D+86%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-312">Voter 312's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 312 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;4'6"N 84&deg;37'31"W</div>
        <div class="text address"><i>approx.</i> 4767, Webster Way Northwest, Baker Ridge, Cobb County, Georgia, 30101, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.655763,-122.35823776439881&ll=47.655763,-122.35823776439881&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/310.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.06860706713609,-84.62539652934424&ll=34.06860706713609,-84.62539652934424&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/311.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-313">Voter 313's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 313 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;25'38"N 71&deg;14'13"W</div>
        <div class="text address"><i>approx.</i> Hayden Woods, Valleyfield Street, Lexington, Middlesex County, Massachusetts, 02421, United States</div>
        <div class="text dem precinct">Precinct Margin: D+68%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-314">Voter 314's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 314 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;29'19"N 101&deg;55'8"W</div>
        <div class="text address"><i>approx.</i> 4998, 116th Street, Lubbock, Lubbock County, Texas, 79424, United States</div>
        <div class="text gop precinct">Precinct Margin: R+58%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.42724975,-71.23696003684003&ll=42.42724975,-71.23696003684003&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/312.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.4887301,-101.91908690509013&ll=33.4887301,-101.91908690509013&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/313.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-315">Voter 315's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 315 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;45'7"N 117&deg;8'8"W</div>
        <div class="text address"><i>approx.</i> 2629, Polk Avenue, University Heights, North Park, San Diego, San Diego County, California, 92104, United States</div>
        <div class="text dem precinct">Precinct Margin: D+74%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-316">Voter 316's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 316 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;31'21"N 79&deg;45'4"W</div>
        <div class="text address"><i>approx.</i> 900, Hilltop Lane, Eden, Rockingham County, North Carolina, 27288, United States</div>
        <div class="text gop precinct">Precinct Margin: R+22%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.752011,-117.135584&ll=32.752011,-117.135584&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/314.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.522571,-79.751385&ll=36.522571,-79.751385&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/315.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-317">Voter 317's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 317 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;55'43"N 122&deg;18'54"W</div>
        <div class="text address"><i>approx.</i> 2034, Harper Street, El Cerrito, Contra Costa County, California, 94530, United States</div>
        <div class="text dem precinct">Precinct Margin: D+76%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-318">Voter 318's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 318 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;8'14"N 74&deg;39'23"W</div>
        <div class="text address"><i>approx.</i> Chesterfield Elementary School, 30, Saddle Way, Villages at Chesterfield Greens, Chesterfield Township, Burlington County, New Jersey, 08515, United States</div>
        <div class="text dem precinct">Precinct Margin: D+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.928668075800715,-122.3150001883452&ll=37.928668075800715,-122.3150001883452&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/316.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.13723725,-74.65651806426729&ll=40.13723725,-74.65651806426729&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/317.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-319">Voter 319's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 319 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;24'15"N 111&deg;46'25"W</div>
        <div class="text address"><i>approx.</i> Sun Circle Trail, Lindsay Crossing, Gilbert, Mesa, Maricopa County, Arizona, 85203, United States</div>
        <div class="text gop precinct">Precinct Margin: R+17%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-320">Voter 320's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 320 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;49'57"N 86&deg;10'23"W</div>
        <div class="text address"><i>approx.</i> 4280, Crown Street, Mapleton, Indianapolis, Marion County, Indiana, 46208, United States</div>
        <div class="text dem precinct">Precinct Margin: D+71%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.40422581169828,-111.7737026361309&ll=33.40422581169828,-111.7737026361309&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/318.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.832731653061224,-86.1732217142857&ll=39.832731653061224,-86.1732217142857&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/319.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-321">Voter 321's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 321 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;52'22"N 77&deg;12'43"W</div>
        <div class="text address"><i>approx.</i> 2824, Wickersham Way, West Falls Church, Fairfax County, Virginia, 22042, United States</div>
        <div class="text dem precinct">Precinct Margin: D+52%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-322">Voter 322's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 322 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;56'8"N 83&deg;38'9"W</div>
        <div class="text address"><i>approx.</i> 10800, Walker Street, Grand Blanc, Genesee County, Michigan, 48439, United States</div>
        <div class="text dem precinct">Precinct Margin: D+7%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.87296954863618,-77.21209725194728&ll=38.87296954863618,-77.21209725194728&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/320.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.935604,-83.635988&ll=42.935604,-83.635988&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/321.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-323">Voter 323's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 323 voted for Biden</div> -->
        <div class="text coordinate"> 28&deg;17'54"N 81&deg;39'52"W</div>
        <div class="text address"><i>approx.</i> 144, Kings Ridge Loop, The Ridge, Polk County, Florida, 33897, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-324">Voter 324's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 324 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;38'44"N 121&deg;19'14"W</div>
        <div class="text address"><i>approx.</i> 4544, Onyx Way, Sacramento County, California, 95608, United States</div>
        <div class="text dem precinct">Precinct Margin: D+12%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=28.298348481587823,-81.66447080377031&ll=28.298348481587823,-81.66447080377031&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/322.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.64556429572032,-121.32057109536998&ll=38.64556429572032,-121.32057109536998&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/323.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-325">Voter 325's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 325 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;12'11"N 80&deg;8'48"W</div>
        <div class="text address"><i>approx.</i> Cypress Creek Road, Fort Lauderdale, Broward County, Florida, 33334, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-326">Voter 326's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 326 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;31'53"N 92&deg;12'0"W</div>
        <div class="text address"><i>approx.</i> 198, Davis Lane, Pine Grove, Ouachita Parish, Louisiana, 71291, United States</div>
        <div class="text gop precinct">Precinct Margin: R+73%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.203318761158492,-80.14683630350304&ll=26.203318761158492,-80.14683630350304&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/324.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.531494,-92.200055&ll=32.531494,-92.200055&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/325.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-327">Voter 327's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 327 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;28'34"N 87&deg;29'2"W</div>
        <div class="text address"><i>approx.</i> 2557, Ticonderoga Street, Schererville, Lake County, Indiana, 46375, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-328">Voter 328's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 328 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;48'47"N 79&deg;44'3"W</div>
        <div class="text address"><i>approx.</i> 31, Seagrass Lane, Isle of Palms, Charleston County, South Carolina, 29451, United States</div>
        <div class="text gop precinct">Precinct Margin: R+22%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.4763406468156,-87.48413668564689&ll=41.4763406468156,-87.48413668564689&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/326.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.8132309,-79.7343469&ll=32.8132309,-79.7343469&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/327.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-329">Voter 329's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 329 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;27'1"N 77&deg;22'47"W</div>
        <div class="text address"><i>approx.</i> Liberty Road, Frederick, Frederick County, Maryland, 21701, United States</div>
        <div class="text dem precinct">Precinct Margin: D+34%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-330">Voter 330's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 330 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;46'33"N 102&deg;57'15"W</div>
        <div class="text address"><i>approx.</i> 3435, County Road U, Deaf Smith County, Texas, 79035, United States</div>
        <div class="text gop precinct">Precinct Margin: R+61%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.45031458779319,-77.3797291763742&ll=39.45031458779319,-77.3797291763742&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/328.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.776085465467794,-102.95429616238167&ll=34.776085465467794,-102.95429616238167&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/329.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-331">Voter 331's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 331 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;32'54"N 105&deg;5'7"W</div>
        <div class="text address"><i>approx.</i> Meadowlark Church of Christ, 2810, Meadowlark Avenue, Meadow Lark Heights, Fort Collins, Larimer County, Colorado, 80526, United States</div>
        <div class="text dem precinct">Precinct Margin: D+40%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-332">Voter 332's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 332 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;14'21"N 81&deg;14'8"W</div>
        <div class="text address"><i>approx.</i> HiHo Hill, Coldbrook, Effingham County, Georgia, 31326, United States</div>
        <div class="text gop precinct">Precinct Margin: R+46%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.54854075,-105.08540695922386&ll=40.54854075,-105.08540695922386&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/330.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.2394381,-81.2357078&ll=32.2394381,-81.2357078&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/331.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-333">Voter 333's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 333 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;7'27"N 87&deg;49'46"W</div>
        <div class="text address"><i>approx.</i> 2006, Illinois Road, Northbrook Park, Northbrook, Northfield Township, Cook County, Illinois, 60062, United States</div>
        <div class="text dem precinct">Precinct Margin: D+36%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-334">Voter 334's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 334 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;40'26"N 76&deg;13'27"W</div>
        <div class="text address"><i>approx.</i> 2115, West Norwegian Street, Yorkville, Pottsville, Schuylkill County, Pennsylvania, 17901, United States</div>
        <div class="text dem precinct">Precinct Margin: D+0%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.12427292791226,-87.82953501297285&ll=42.12427292791226,-87.82953501297285&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/332.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.67406131965036,-76.22439408794902&ll=40.67406131965036,-76.22439408794902&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/333.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-335">Voter 335's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 335 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;32'32"N 80&deg;11'17"W</div>
        <div class="text address"><i>approx.</i> 99, Linden Place, Sewickley, Allegheny County, Pennsylvania, 15143, United States</div>
        <div class="text dem precinct">Precinct Margin: D+19%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-336">Voter 336's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 336 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;30'37"N 81&deg;25'40"W</div>
        <div class="text address"><i>approx.</i> 6601, Norvale Circle West, Gates Mills, Cuyahoga County, Ohio, 44040, United States</div>
        <div class="text dem precinct">Precinct Margin: D+1%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.54232,-80.188155&ll=40.54232,-80.188155&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/334.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.5104177,-81.4278309&ll=41.5104177,-81.4278309&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/335.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-337">Voter 337's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 337 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;43'9"N 80&deg;8'41"W</div>
        <div class="text address"><i>approx.</i> Brush Creek Water Pollution Control Facility, 2306, Powell Road, Cranberry Township, Butler County, Pennsylvania, 16066, United States</div>
        <div class="text gop precinct">Precinct Margin: R+11%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-338">Voter 338's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 338 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;10'22"N 86&deg;12'12"W</div>
        <div class="text address"><i>approx.</i> 3413, Eastover Road, Wilson County, Tennessee, 37184, United States</div>
        <div class="text gop precinct">Precinct Margin: R+57%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.719339149999996,-80.14491987629435&ll=40.719339149999996,-80.14491987629435&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/336.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.17296152376636,-86.20349676161256&ll=36.17296152376636,-86.20349676161256&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/337.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-339">Voter 339's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 339 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;3'9"N 105&deg;18'40"W</div>
        <div class="text address"><i>approx.</i> 18, Pine Brook Road, Pine Brook Hill, Boulder County, Colorado, 80304, United States</div>
        <div class="text dem precinct">Precinct Margin: D+70%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-340">Voter 340's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 340 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;56'54"N 76&deg;7'33"W</div>
        <div class="text address"><i>approx.</i> Battlefield History Trail, Sackets Harbor, Hounsfield, Jefferson County, New York, 13685, United States</div>
        <div class="text dem precinct">Precinct Margin: D+11%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.0525429,-105.311292129507&ll=40.0525429,-105.311292129507&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/338.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.948547,-76.126063&ll=43.948547,-76.126063&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/339.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-341">Voter 341's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 341 voted for Biden</div> -->
        <div class="text coordinate"> 47&deg;43'25"N 122&deg;11'11"W</div>
        <div class="text address"><i>approx.</i> 13806, 116th Avenue Northeast, Kingsgate, Kirkland, King County, Washington, 98034, United States</div>
        <div class="text dem precinct">Precinct Margin: D+25%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-342">Voter 342's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 342 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;8'9"N 78&deg;55'14"W</div>
        <div class="text address"><i>approx.</i> 7313, Johnson Mill Road, Durham County, North Carolina, 27503, United States</div>
        <div class="text dem precinct">Precinct Margin: D+4%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.723825250000004,-122.1865809&ll=47.723825250000004,-122.1865809&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/340.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.135863673493255,-78.92075668379348&ll=36.135863673493255,-78.92075668379348&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/341.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-343">Voter 343's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 343 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;26'15"N 83&deg;21'11"W</div>
        <div class="text address"><i>approx.</i> Norfolk Street, Livonia, Wayne County, Michigan, 48152, United States</div>
        <div class="text dem precinct">Precinct Margin: D+10%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-344">Voter 344's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 344 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;32'45"N 74&deg;45'8"W</div>
        <div class="text address"><i>approx.</i> Weymouth Elwood Road, Hamilton Township, Atlantic County, New Jersey, 08330, United States</div>
        <div class="text dem precinct">Precinct Margin: D+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.43762175601033,-83.35328341158069&ll=42.43762175601033,-83.35328341158069&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/342.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.545927132348844,-74.75228962360994&ll=39.545927132348844,-74.75228962360994&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/343.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-345">Voter 345's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 345 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;59'55"N 92&deg;28'35"W</div>
        <div class="text address"><i>approx.</i> 527, Round Mountain Road, Round Mountain, Faulkner County, Arkansas, 72034, United States</div>
        <div class="text gop precinct">Precinct Margin: R+18%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-346">Voter 346's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 346 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;16'22"N 81&deg;48'59"W</div>
        <div class="text address"><i>approx.</i> 3672, Lennox Circle, Brunswick, Medina County, Ohio, 44212, United States</div>
        <div class="text gop precinct">Precinct Margin: R+3%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.998775,-92.476651&ll=34.998775,-92.476651&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/344.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.27288766857083,-81.81662362094929&ll=41.27288766857083,-81.81662362094929&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/345.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-347">Voter 347's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 347 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;27'24"N 112&deg;15'36"W</div>
        <div class="text address"><i>approx.</i> 9399, West Baden Street, Tolleson, Maricopa County, Arizona, 85353, United States</div>
        <div class="text dem precinct">Precinct Margin: D+52%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-348">Voter 348's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 348 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;49'45"N 88&deg;8'50"W</div>
        <div class="text address"><i>approx.</i> 25875, West Loomis Road, Wind Lake, Town of Norway, Racine County, Wisconsin, 53185, United States</div>
        <div class="text gop precinct">Precinct Margin: R+48%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.4568187,-112.2602112&ll=33.4568187,-112.2602112&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/346.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.829413580917965,-88.14732149674822&ll=42.829413580917965,-88.14732149674822&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/347.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-349">Voter 349's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 349 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;57'31"N 122&deg;3'6"W</div>
        <div class="text address"><i>approx.</i> Iron Horse Trail, Sherman Acres, Pleasant Hill, Contra Costa County, California, 1355, United States</div>
        <div class="text dem precinct">Precinct Margin: D+49%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-350">Voter 350's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 350 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;59'23"N 82&deg;43'45"W</div>
        <div class="text address"><i>approx.</i> 6849, Mink Street Southwest, Wagram, Pataskala, Etna Township, Licking County, Ohio, 43062, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.958709372156406,-122.0517987273514&ll=37.958709372156406,-122.0517987273514&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/348.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.989910334857996,-82.72916858996324&ll=39.989910334857996,-82.72916858996324&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/349.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-351">Voter 351's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 351 voted for Biden</div> -->
        <div class="text coordinate"> 31&deg;17'18"N 81&deg;20'59"W</div>
        <div class="text address"><i>approx.</i> 934, Champney, Glynn County, Georgia, 31522, United States</div>
        <div class="text gop precinct">Precinct Margin: R+36%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-352">Voter 352's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 352 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;37'14"N 120&deg;19'32"W</div>
        <div class="text address"><i>approx.</i> Prouty Road, Mariposa County, California, United States</div>
        <div class="text gop precinct">Precinct Margin: R+34%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=31.28844716786182,-81.3497651189595&ll=31.28844716786182,-81.3497651189595&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/350.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.620739,-120.325712&ll=37.620739,-120.325712&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/351.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-353">Voter 353's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 353 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;51'0"N 121&deg;58'31"W</div>
        <div class="text address"><i>approx.</i> 3257, Stone Valley Road, Contra Costa County, California, 94507, United States</div>
        <div class="text dem precinct">Precinct Margin: D+22%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-354">Voter 354's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 354 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;3'17"N 73&deg;33'16"W</div>
        <div class="text address"><i>approx.</i> 32, Merrell Avenue, Stamford, Fairfield County, Connecticut, 06902, United States</div>
        <div class="text dem precinct">Precinct Margin: D+65%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.850032569333756,-121.9753439746334&ll=37.850032569333756,-121.9753439746334&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/352.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.0547383,-73.5547127&ll=41.0547383,-73.5547127&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/353.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-355">Voter 355's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 355 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;51'2"N 77&deg;3'5"W</div>
        <div class="text address"><i>approx.</i> Capital Bikeshare, 26th Street South, National Landing, Crystal City, Arlington, Arlington County, Virginia, 22202, United States</div>
        <div class="text dem precinct">Precinct Margin: D+61%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-356">Voter 356's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 356 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;54'44"N 75&deg;10'49"W</div>
        <div class="text address"><i>approx.</i> 1917, Schley Street, Packer Park, South Philadelphia, Philadelphia, Philadelphia County, Pennsylvania, 19145, United States</div>
        <div class="text gop precinct">Precinct Margin: R+44%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.8506389,-77.0514605&ll=38.8506389,-77.0514605&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/354.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.912379200000004,-75.18052052632272&ll=39.912379200000004,-75.18052052632272&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/355.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-357">Voter 357's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 357 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;5'6"N 94&deg;36'5"W</div>
        <div class="text address"><i>approx.</i> 2325, Fairmount Avenue, West Side, Kansas City, Jackson County, Missouri, 64108, United States</div>
        <div class="text dem precinct">Precinct Margin: D+65%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-358">Voter 358's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 358 voted for Trump</div> -->
        <div class="text coordinate"> 47&deg;45'52"N 117&deg;24'13"W</div>
        <div class="text address"><i>approx.</i> Pine Acres Par 3, East Regina Road, Fairwood, Spokane County, Washington, 99218-1929, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.08514735,-94.60162105&ll=39.08514735,-94.60162105&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/356.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.764542399999996,-117.40372601600731&ll=47.764542399999996,-117.40372601600731&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/357.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-359">Voter 359's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 359 voted for Biden</div> -->
        <div class="text coordinate"> 46&deg;50'51"N 92&deg;6'21"W</div>
        <div class="text address"><i>approx.</i> Ridgeview Country Club, 700, West Redwing Street, Duluth, Saint Louis County, Minnesota, 55803, United States</div>
        <div class="text dem precinct">Precinct Margin: D+31%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-360">Voter 360's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 360 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;4'15"N 84&deg;11'47"W</div>
        <div class="text address"><i>approx.</i> 191, Orchard Crest Circle, Anderson County, Tennessee, 37716, United States</div>
        <div class="text gop precinct">Precinct Margin: R+44%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=46.8477327,-92.10593437128854&ll=46.8477327,-92.10593437128854&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/358.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.07087477721194,-84.19649683393929&ll=36.07087477721194,-84.19649683393929&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/359.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-361">Voter 361's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 361 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;26'26"N 97&deg;44'59"W</div>
        <div class="text address"><i>approx.</i> 6511, Melrose Trail, Milwood, Austin, Travis County, Texas, 78727, United States</div>
        <div class="text dem precinct">Precinct Margin: D+48%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-362">Voter 362's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 362 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;58'57"N 77&deg;31'23"W</div>
        <div class="text address"><i>approx.</i> Bobolink Alley, Brambleton, Loudoun County, Virginia, 20107, United States</div>
        <div class="text dem precinct">Precinct Margin: D+32%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.4407992,-97.74974025091907&ll=30.4407992,-97.74974025091907&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/360.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.982774,-77.523238&ll=38.982774,-77.523238&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/361.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-363">Voter 363's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 363 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;45'46"N 111&deg;34'38"W</div>
        <div class="text address"><i>approx.</i> 5192, Homestead Road, Jeremy Ranch, Summit County, Utah, 84098, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-364">Voter 364's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 364 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;5'3"N 77&deg;27'10"W</div>
        <div class="text address"><i>approx.</i> 17, Clarkes Crossing, Village of Fairport, Town of Perinton, Monroe County, New York, 14450, United States</div>
        <div class="text dem precinct">Precinct Margin: D+22%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.76296898508681,-111.5773912071034&ll=40.76296898508681,-111.5773912071034&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/362.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.0843975,-77.4530325&ll=43.0843975,-77.4530325&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/363.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-365">Voter 365's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 365 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;57'21"N 70&deg;40'5"W</div>
        <div class="text address"><i>approx.</i> 35, Russell Street, Plymouth, Plymouth County, Massachusetts, 02360, United States</div>
        <div class="text dem precinct">Precinct Margin: D+29%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-366">Voter 366's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 366 voted for Trump</div> -->
        <div class="text coordinate"> 25&deg;37'16"N 80&deg;26'23"W</div>
        <div class="text address"><i>approx.</i> 15264, Southwest 156th Terrace, Miami-Dade County, Florida, 33187, United States</div>
        <div class="text gop precinct">Precinct Margin: R+3%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.9559365,-70.6680569968062&ll=41.9559365,-70.6680569968062&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/364.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=25.6213024,-80.439993&ll=25.6213024,-80.439993&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/365.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-367">Voter 367's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 367 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;0'17"N 118&deg;16'7"W</div>
        <div class="text address"><i>approx.</i> 379, East 43rd Place, Historic South-Central, Los Angeles, Los Angeles County, California, 90011, United States</div>
        <div class="text dem precinct">Precinct Margin: D+72%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-368">Voter 368's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 368 voted for Trump</div> -->
        <div class="text coordinate"> 47&deg;19'11"N 122&deg;24'39"W</div>
        <div class="text address"><i>approx.</i> Picnic Point Beach Trail, Federal Way, King County, Washington, United States</div>
        <div class="text dem precinct">Precinct Margin: D+23%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.004849365079366,-118.2687768888889&ll=34.004849365079366,-118.2687768888889&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/366.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.319982225073936,-122.41106621058572&ll=47.319982225073936,-122.41106621058572&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/367.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-369">Voter 369's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 369 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;9'17"N 117&deg;17'47"W</div>
        <div class="text address"><i>approx.</i> 3414, Four Peaks Street, The Foothills, Calavera, Carlsbad, San Diego County, California, 92010, United States</div>
        <div class="text dem precinct">Precinct Margin: D+15%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-370">Voter 370's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 370 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;13'42"N 92&deg;39'25"W</div>
        <div class="text address"><i>approx.</i> Song of Praise Church, 4th Street, Jennings, Jefferson Davis Parish, Louisiana, 70546, United States</div>
        <div class="text gop precinct">Precinct Margin: R+68%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.15484485,-117.2964573394478&ll=33.15484485,-117.2964573394478&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/368.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.2285394,-92.657085&ll=30.2285394,-92.657085&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/369.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-371">Voter 371's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 371 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;57'20"N 117&deg;17'42"W</div>
        <div class="text address"><i>approx.</i> 21069, Penunuri Place, Moreno Valley, Riverside County, California, 92557, United States</div>
        <div class="text dem precinct">Precinct Margin: D+30%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-372">Voter 372's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 372 voted for Trump</div> -->
        <div class="text coordinate"> 25&deg;46'47"N 80&deg;24'20"W</div>
        <div class="text address"><i>approx.</i> 12932, Northwest 10th Street, Miami-Dade County, Florida, 33182, United States</div>
        <div class="text gop precinct">Precinct Margin: R+24%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.95573405376683,-117.29508454517422&ll=33.95573405376683,-117.29508454517422&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/370.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=25.7799488,-80.4056589987596&ll=25.7799488,-80.4056589987596&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/371.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-373">Voter 373's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 373 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;1'18"N 80&deg;14'42"W</div>
        <div class="text address"><i>approx.</i> 7845, Northwest 15th Street, Pembroke Pines, Broward County, Florida, 33024, United States</div>
        <div class="text dem precinct">Precinct Margin: D+7%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-374">Voter 374's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 374 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;22'11"N 118&deg;56'15"W</div>
        <div class="text address"><i>approx.</i> 763, Crane Street, Kern County, California, 93306, United States</div>
        <div class="text dem precinct">Precinct Margin: D+31%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.02177752259668,-80.2451588188838&ll=26.02177752259668,-80.2451588188838&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/372.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.369860653061224,-118.9376138367347&ll=35.369860653061224,-118.9376138367347&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/373.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-375">Voter 375's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 375 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;43'30"N 71&deg;27'56"W</div>
        <div class="text address"><i>approx.</i> 11, Warton Road, Ward 8, Nashua, Hillsborough County, New Hampshire, 03062, United States</div>
        <div class="text dem precinct">Precinct Margin: D+27%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-376">Voter 376's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 376 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;19'48"N 88&deg;17'6"W</div>
        <div class="text address"><i>approx.</i> Slinger High School, Beine Street, Slinger, Washington County, Wisconsin, 53086, United States</div>
        <div class="text gop precinct">Precinct Margin: R+43%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.72522655,-71.46580552867584&ll=42.72522655,-71.46580552867584&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/374.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.330258650000005,-88.28513928386737&ll=43.330258650000005,-88.28513928386737&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/375.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-377">Voter 377's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 377 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;47'35"N 83&deg;42'11"W</div>
        <div class="text address"><i>approx.</i> Millpond Manor, 201, East Elizabeth Street, Fenton, Genesee County, Michigan, 48430, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-378">Voter 378's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 378 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;27'18"N 88&deg;0'14"W</div>
        <div class="text address"><i>approx.</i> 23822, Manhattan Road, Will County, Illinois, 60442, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.793279600000005,-83.70330040117712&ll=42.793279600000005,-83.70330040117712&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/376.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.455033797691684,-88.0039504990638&ll=41.455033797691684,-88.0039504990638&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/377.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-379">Voter 379's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 379 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;57'16"N 81&deg;34'50"W</div>
        <div class="text address"><i>approx.</i> 8538, Beverly Lane, Saint Johns County, Florida, 32092, United States</div>
        <div class="text gop precinct">Precinct Margin: R+50%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-380">Voter 380's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 380 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;55'31"N 83&deg;42'58"W</div>
        <div class="text address"><i>approx.</i> 7143, Brewer Road, Mundy Township, Genesee County, Michigan, 48507, United States</div>
        <div class="text dem precinct">Precinct Margin: D+5%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.9544786,-81.5806075&ll=29.9544786,-81.5806075&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/378.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.925464350812206,-83.7161634296036&ll=42.925464350812206,-83.7161634296036&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/379.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-381">Voter 381's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 381 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;14'2"N 110&deg;53'56"W</div>
        <div class="text address"><i>approx.</i> 990, North Catalina Avenue, Tucson, Pima County, Arizona, 85711, United States</div>
        <div class="text dem precinct">Precinct Margin: D+49%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-382">Voter 382's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 382 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;55'45"N 76&deg;14'5"W</div>
        <div class="text address"><i>approx.</i> 8424, Radnor Road, Brookfield Park, Norfolk, Virginia, 23503, United States</div>
        <div class="text dem precinct">Precinct Margin: D+1%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.23412843077302,-110.8989309374444&ll=32.23412843077302,-110.8989309374444&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/380.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.9292945,-76.23498024904694&ll=36.9292945,-76.23498024904694&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/381.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-383">Voter 383's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 383 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;38'47"N 74&deg;11'40"W</div>
        <div class="text address"><i>approx.</i> 163, Butler Street, Liberty Square, Elizabeth, Union County, New Jersey, 07206, United States</div>
        <div class="text dem precinct">Precinct Margin: D+53%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-384">Voter 384's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 384 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;6'26"N 119&deg;34'3"W</div>
        <div class="text address"><i>approx.</i> Letts Street, Corcoran, Kings County, California, 93212, United States</div>
        <div class="text dem precinct">Precinct Margin: D+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.646483363636364,-74.19457154545455&ll=40.646483363636364,-74.19457154545455&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/382.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.10734566932498,-119.56760881261823&ll=36.10734566932498,-119.56760881261823&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/383.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-385">Voter 385's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 385 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;56'53"N 84&deg;54'27"W</div>
        <div class="text address"><i>approx.</i> 4575, Mc Pherson Church Road, Hanlin, Paulding County, Georgia, 30132, United States</div>
        <div class="text gop precinct">Precinct Margin: R+17%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-386">Voter 386's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 386 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;46'12"N 73&deg;57'22"W</div>
        <div class="text address"><i>approx.</i> 310, East 75th Street, Manhattan Community Board 8, Lenox Hill, New York County, City of New York, New York, 10021, United States</div>
        <div class="text dem precinct">Precinct Margin: D+57%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.948191,-84.907661&ll=33.948191,-84.907661&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/384.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.7701297,-73.9562100497117&ll=40.7701297,-73.9562100497117&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/385.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-387">Voter 387's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 387 voted for Biden</div> -->
        <div class="text coordinate"> 47&deg;41'1"N 116&deg;46'18"W</div>
        <div class="text address"><i>approx.</i> 1019, North 10th Street, Coeur d'Alene, Kootenai County, Idaho, 83814, United States</div>
        <div class="text dem precinct">Precinct Margin: D+6%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-388">Voter 388's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 388 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;6'14"N 117&deg;51'55"W</div>
        <div class="text address"><i>approx.</i> 5036, Arroway Avenue, Covina, Los Angeles County, California, 91724, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.683837,-116.771726&ll=47.683837,-116.771726&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/386.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.10415806319042,-117.86534336560831&ll=34.10415806319042,-117.86534336560831&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/387.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-389">Voter 389's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 389 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;46'2"N 77&deg;47'17"W</div>
        <div class="text address"><i>approx.</i> 610, South Academy Street, Boalsburg, Harris Township, Centre County, Pennsylvania, 16827, United States</div>
        <div class="text dem precinct">Precinct Margin: D+21%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-390">Voter 390's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 390 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;56'35"N 76&deg;42'57"W</div>
        <div class="text address"><i>approx.</i> Irving Road, South Wood Hills, York, Spring Garden Township, York County, Pennsylvania, 17403, United States</div>
        <div class="text dem precinct">Precinct Margin: D+16%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.76748095,-77.78813874185823&ll=40.76748095,-77.78813874185823&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/388.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.943258611803934,-76.71594965395562&ll=39.943258611803934,-76.71594965395562&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/389.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-391">Voter 391's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 391 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;4'38"N 108&deg;35'51"W</div>
        <div class="text address"><i>approx.</i> 473, Dike Road, Grand Junction, Mesa County, Colorado, 81507, United States</div>
        <div class="text gop precinct">Precinct Margin: R+6%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-392">Voter 392's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 392 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;7'6"N 88&deg;15'0"W</div>
        <div class="text address"><i>approx.</i> 18, Sparrow Road, Carpentersville, Dundee Township, Kane County, Illinois, 60110, United States</div>
        <div class="text dem precinct">Precinct Margin: D+37%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.07736336200436,-108.59771571519008&ll=39.07736336200436,-108.59771571519008&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/390.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.11853710712898,-88.25027483404763&ll=42.11853710712898,-88.25027483404763&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/391.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-393">Voter 393's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 393 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;21'28"N 95&deg;16'41"W</div>
        <div class="text address"><i>approx.</i> 2719, Pine Avenue, Orchard Park, Parsons, Labette County, Kansas, 67357, United States</div>
        <div class="text gop precinct">Precinct Margin: R+10%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-394">Voter 394's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 394 voted for Trump</div> -->
        <div class="text coordinate"> 48&deg;26'27"N 122&deg;20'9"W</div>
        <div class="text address"><i>approx.</i> 330, Pacific Place, Mount Vernon, Skagit County, Washington, 98273, United States</div>
        <div class="text dem precinct">Precinct Margin: D+14%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.357813097298084,-95.27829409169027&ll=37.357813097298084,-95.27829409169027&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/392.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=48.440858750000004,-122.33608141053676&ll=48.440858750000004,-122.33608141053676&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/393.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-395">Voter 395's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 395 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;45'5"N 122&deg;53'16"W</div>
        <div class="text address"><i>approx.</i> 33052, Southwest Keyes Road, Scappoose, Columbia County, Oregon, 97056, United States</div>
        <div class="text dem precinct">Precinct Margin: D+7%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-396">Voter 396's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 396 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;52'1"N 77&deg;27'13"W</div>
        <div class="text address"><i>approx.</i> 5240, Glen Meadow Road, Centreville, Fairfax County, Virginia, 20120, United States</div>
        <div class="text dem precinct">Precinct Margin: D+31%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.751593590013115,-122.88777885323215&ll=45.751593590013115,-122.88777885323215&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/394.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.86715517054399,-77.453625485044&ll=38.86715517054399,-77.453625485044&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/395.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-397">Voter 397's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 397 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;0'19"N 91&deg;49'16"W</div>
        <div class="text address"><i>approx.</i> 178, West Washington Street, Leesburg, New Iberia, Iberia Parish, Louisiana, 70560, United States</div>
        <div class="text dem precinct">Precinct Margin: D+56%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-398">Voter 398's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 398 voted for Trump</div> -->
        <div class="text coordinate"> 31&deg;10'50"N 94&deg;22'5"W</div>
        <div class="text address"><i>approx.</i> 282, Dogwood Glen Street, Angelina County, Texas, 75980, United States</div>
        <div class="text gop precinct">Precinct Margin: R+81%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.005482108108108,-91.8212847027027&ll=30.005482108108108,-91.8212847027027&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/396.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=31.180642304268318,-94.3682409885422&ll=31.180642304268318,-94.3682409885422&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/397.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-399">Voter 399's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 399 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;58'9"N 83&deg;22'40"W</div>
        <div class="text address"><i>approx.</i> 1005, College Avenue, Athens-Clarke County Unified Government, Athens-Clarke County, Georgia, 30601, United States</div>
        <div class="text dem precinct">Precinct Margin: D+61%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-400">Voter 400's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 400 voted for Trump</div> -->
        <div class="text coordinate"> 28&deg;29'48"N 82&deg;32'1"W</div>
        <div class="text address"><i>approx.</i> 4399, Goldcoast Avenue, Spring Hill, Hernando County, Florida, 34609, United States</div>
        <div class="text gop precinct">Precinct Margin: R+18%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.9692075,-83.3780353&ll=33.9692075,-83.3780353&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/398.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=28.4967875,-82.53363975&ll=28.4967875,-82.53363975&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/399.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-401">Voter 401's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 401 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;32'24"N 74&deg;6'40"W</div>
        <div class="text address"><i>approx.</i> 550, South Plank Road, Orange Lake, Village of Walden, Town of Newburgh, Orange County, New York, 12586, United States</div>
        <div class="text dem precinct">Precinct Margin: D+8%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-402">Voter 402's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 402 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;28'21"N 93&deg;11'8"W</div>
        <div class="text address"><i>approx.</i> 340, 85th Avenue, Marion County, Iowa, 50170, United States</div>
        <div class="text gop precinct">Precinct Margin: R+44%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.54012397251382,-74.11135859240363&ll=41.54012397251382,-74.11135859240363&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/400.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.47260187947069,-93.18561683847804&ll=41.47260187947069,-93.18561683847804&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/401.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-403">Voter 403's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 403 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;44'30"N 87&deg;35'6"W</div>
        <div class="text address"><i>approx.</i> 8401-8423, South Stony Island Avenue, Pierces Park, Avalon Park, Chicago, Hyde Park Township, Cook County, Illinois, 60617, United States</div>
        <div class="text dem precinct">Precinct Margin: D+91%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-404">Voter 404's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 404 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;32'58"N 85&deg;7'36"W</div>
        <div class="text address"><i>approx.</i> 199, County Road 770, Lee County, Alabama, 36877, United States</div>
        <div class="text gop precinct">Precinct Margin: R+38%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.74192655,-87.58503699861694&ll=41.74192655,-87.58503699861694&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/402.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.549528,-85.126698&ll=32.549528,-85.126698&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/403.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-405">Voter 405's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 405 voted for Biden</div> -->
        <div class="text coordinate"> 47&deg;42'17"N 117&deg;23'18"W</div>
        <div class="text address"><i>approx.</i> 1516, East Crown Avenue, Whitman, Hillyard, Spokane, Spokane County, Washington, 99207, United States</div>
        <div class="text dem precinct">Precinct Margin: D+2%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-406">Voter 406's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 406 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;21'34"N 98&deg;25'27"W</div>
        <div class="text address"><i>approx.</i> 855, Utopia Lane, San Antonio, Bexar County, Texas, 78223, United States</div>
        <div class="text dem precinct">Precinct Margin: D+24%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.70473052142685,-117.38834849441295&ll=47.70473052142685,-117.38834849441295&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/404.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.359666408163267,-98.42424773469388&ll=29.359666408163267,-98.42424773469388&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/405.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-407">Voter 407's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 407 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;0'28"N 93&deg;23'38"W</div>
        <div class="text address"><i>approx.</i> 2723, Ensign Circle, New Hope, Hennepin County, Minnesota, 55427, United States</div>
        <div class="text dem precinct">Precinct Margin: D+42%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-408">Voter 408's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 408 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;52'18"N 90&deg;7'11"W</div>
        <div class="text address"><i>approx.</i> 699, Niagara Street, East Alton, Madison County, Illinois, 62024, United States</div>
        <div class="text gop precinct">Precinct Margin: R+23%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.00803203061224,-93.3939828877551&ll=45.00803203061224,-93.3939828877551&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/406.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.871789,-90.1199239&ll=38.871789,-90.1199239&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/407.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-409">Voter 409's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 409 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;51'30"N 77&deg;12'27"W</div>
        <div class="text address"><i>approx.</i> 7609, Trail Run Road, West Falls Church, Fairfax County, Virginia, 22042, United States</div>
        <div class="text dem precinct">Precinct Margin: D+39%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-410">Voter 410's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 410 voted for Trump</div> -->
        <div class="text coordinate"> 28&deg;49'8"N 81&deg;9'35"W</div>
        <div class="text address"><i>approx.</i> Yellow Trail, Volusia County, Florida, 32764, United States</div>
        <div class="text gop precinct">Precinct Margin: R+58%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.858360523286564,-77.2077529143351&ll=38.858360523286564,-77.2077529143351&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/408.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=28.819025041075022,-81.1598532947509&ll=28.819025041075022,-81.1598532947509&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/409.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-411">Voter 411's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 411 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;35'25"N 72&deg;54'26"W</div>
        <div class="text address"><i>approx.</i> 18, Great Pine Path, Marion Historic District, Southington, Hartford County, Connecticut, 06479, United States</div>
        <div class="text gop precinct">Precinct Margin: R+4%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-412">Voter 412's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 412 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;9'27"N 91&deg;23'5"W</div>
        <div class="text address"><i>approx.</i> 4898, County Road 424, Pike County, Missouri, 63359, United States</div>
        <div class="text gop precinct">Precinct Margin: R+68%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.5904701,-72.9073944&ll=41.5904701,-72.9073944&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/410.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.157599,-91.384906&ll=39.157599,-91.384906&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/411.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-413">Voter 413's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 413 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;6'18"N 93&deg;20'59"W</div>
        <div class="text address"><i>approx.</i> 8345, Scott Avenue North, Brooklyn Park, Hennepin County, Minnesota, 55443, United States</div>
        <div class="text dem precinct">Precinct Margin: D+45%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-414">Voter 414's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 414 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;8'51"N 89&deg;22'44"W</div>
        <div class="text address"><i>approx.</i> 399, South Adams Street, Lincoln, Logan County, Illinois, 62656, United States</div>
        <div class="text gop precinct">Precinct Margin: R+23%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.105046798203894,-93.34975835969311&ll=45.105046798203894,-93.34975835969311&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/412.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.147734,-89.379018&ll=40.147734,-89.379018&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/413.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-415">Voter 415's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 415 voted for Biden</div> -->
        <div class="text coordinate"> 47&deg;23'15"N 117&deg;10'36"W</div>
        <div class="text address"><i>approx.</i> Zion Lutheran Church, West Hamilton Avenue, Fairfield, Spokane County, Washington, United States</div>
        <div class="text gop precinct">Precinct Margin: R+41%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-416">Voter 416's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 416 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;59'53"N 87&deg;42'11"W</div>
        <div class="text address"><i>approx.</i> 6415-6425, North Richmond Street, West Ridge, Chicago, Rogers Park Township, Cook County, Illinois, 60645, United States</div>
        <div class="text dem precinct">Precinct Margin: D+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.387584950000004,-117.17673522474479&ll=47.387584950000004,-117.17673522474479&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/414.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.9981973,-87.70309277167773&ll=41.9981973,-87.70309277167773&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/415.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-417">Voter 417's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 417 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;22'57"N 95&deg;3'21"W</div>
        <div class="text address"><i>approx.</i> Dump Road, La Marque, Galveston County, Texas, 77510, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-418">Voter 418's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 418 voted for Trump</div> -->
        <div class="text coordinate"> 31&deg;53'8"N 101&deg;57'19"W</div>
        <div class="text address"><i>approx.</i> 10425, South County Road 1138, Midland County, Texas, 79706, United States</div>
        <div class="text gop precinct">Precinct Margin: R+71%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.382571726791273,-95.05591233312543&ll=29.382571726791273,-95.05591233312543&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/416.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=31.885677638000523,-101.95543898869505&ll=31.885677638000523,-101.95543898869505&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/417.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-419">Voter 419's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 419 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;9'54"N 97&deg;8'23"W</div>
        <div class="text address"><i>approx.</i> 869, Sanders Road, Denton, Denton County, Texas, 76210, United States</div>
        <div class="text gop precinct">Precinct Margin: R+14%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-420">Voter 420's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 420 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;59'34"N 79&deg;56'59"W</div>
        <div class="text address"><i>approx.</i> 665, West Main Street, Jamestown, Guilford County, North Carolina, 27282, United States</div>
        <div class="text gop precinct">Precinct Margin: R+3%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.165210263666175,-97.13996607333578&ll=33.165210263666175,-97.13996607333578&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/418.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.9928916122449,-79.9498008367347&ll=35.9928916122449,-79.9498008367347&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/419.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-421">Voter 421's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 421 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;57'19"N 118&deg;7'21"W</div>
        <div class="text address"><i>approx.</i> 9730, Downey Avenue, Downey, Los Angeles County, California, 90240, United States</div>
        <div class="text dem precinct">Precinct Margin: D+26%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-422">Voter 422's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 422 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;34'12"N 98&deg;30'3"W</div>
        <div class="text address"><i>approx.</i> 200, Bluff Hollow, San Antonio, Bexar County, Texas, 78216, United States</div>
        <div class="text dem precinct">Precinct Margin: D+2%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.95528168404055,-118.12272430851598&ll=33.95528168404055,-118.12272430851598&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/420.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.570029,-98.501054&ll=29.570029,-98.501054&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/421.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-423">Voter 423's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 423 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;47'5"N 118&deg;8'49"W</div>
        <div class="text address"><i>approx.</i> 3873, East 14th Street, Long Beach, Los Angeles County, California, 90804, United States</div>
        <div class="text dem precinct">Precinct Margin: D+52%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-424">Voter 424's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 424 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;35'16"N 122&deg;21'5"W</div>
        <div class="text address"><i>approx.</i> 1990, North Washougal River Road, Washougal, Clark County, Washington, 98671, United States</div>
        <div class="text gop precinct">Precinct Margin: R+1%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.78494447580645,-118.14698019354839&ll=33.78494447580645,-118.14698019354839&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/422.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.58778465,-122.35159495356821&ll=45.58778465,-122.35159495356821&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/423.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-425">Voter 425's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 425 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;49'29"N 72&deg;40'30"W</div>
        <div class="text address"><i>approx.</i> 637, Matianuck Avenue, Windsor, Hartford County, Connecticut, 06095, United States</div>
        <div class="text dem precinct">Precinct Margin: D+74%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-426">Voter 426's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 426 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;30'30"N 111&deg;55'29"W</div>
        <div class="text address"><i>approx.</i> FireSky Resort & Spa, East Chaparral Road, Scottsdale, Maricopa County, Arizona, 85251, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.8248698,-72.6751559&ll=41.8248698,-72.6751559&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/424.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.508464399999994,-111.92493367617229&ll=33.508464399999994,-111.92493367617229&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/425.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-427">Voter 427's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 427 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;18'15"N 115&deg;16'37"W</div>
        <div class="text address"><i>approx.</i> Aurora Falls Street, Tule Springs, Las Vegas, Clark County, Nevada, 89159, United States</div>
        <div class="text gop precinct">Precinct Margin: R+16%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-428">Voter 428's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 428 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;7'12"N 82&deg;18'7"W</div>
        <div class="text address"><i>approx.</i> Southwest 110th Avenue, York, Marion County, Florida, 34481, United States</div>
        <div class="text gop precinct">Precinct Margin: R+22%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.30424773379315,-115.2771560608995&ll=36.30424773379315,-115.2771560608995&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/426.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.120200723969248,-82.30209114427059&ll=29.120200723969248,-82.30209114427059&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/427.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-429">Voter 429's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 429 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;20'46"N 79&deg;59'38"W</div>
        <div class="text address"><i>approx.</i> 5147, Curry Road, Baldwin, Allegheny County, Pennsylvania, 15236, United States</div>
        <div class="text dem precinct">Precinct Margin: D+3%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-430">Voter 430's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 430 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;51'50"N 107&deg;48'55"W</div>
        <div class="text address"><i>approx.</i> County Road 102, Moffat County, Colorado, United States</div>
        <div class="text gop precinct">Precinct Margin: R+70%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.34631801941942,-79.99408751321693&ll=40.34631801941942,-79.99408751321693&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/428.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.863943,-107.815414&ll=40.863943,-107.815414&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/429.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-431">Voter 431's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 431 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;0'35"N 80&deg;8'1"W</div>
        <div class="text address"><i>approx.</i> 1351, Van Buren Street, Hollywood, Broward County, Florida, 33019, United States</div>
        <div class="text dem precinct">Precinct Margin: D+23%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-432">Voter 432's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 432 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;16'20"N 81&deg;31'59"W</div>
        <div class="text address"><i>approx.</i> 1942, 20th Street, North Parkersburg, Parkersburg, Wood County, West Virginia, 26101, United States</div>
        <div class="text gop precinct">Precinct Margin: R+31%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.00999985230092,-80.1338029916432&ll=26.00999985230092,-80.1338029916432&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/430.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.27228461630483,-81.53332462175914&ll=39.27228461630483,-81.53332462175914&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/431.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-433">Voter 433's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 433 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;4'42"N 89&deg;53'4"W</div>
        <div class="text address"><i>approx.</i> 2671, Mount Moriah Road, Memphis, Shelby County, Tennessee, 38115, United States</div>
        <div class="text dem precinct">Precinct Margin: D+81%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-434">Voter 434's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 434 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;49'38"N 73&deg;5'17"W</div>
        <div class="text address"><i>approx.</i> 21, Walter Street, Suffolk County, New York, 11741, United States</div>
        <div class="text gop precinct">Precinct Margin: R+24%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.07833949468231,-89.88468666251313&ll=35.07833949468231,-89.88468666251313&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/432.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.8272328,-73.08829110862462&ll=40.8272328,-73.08829110862462&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/433.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-435">Voter 435's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 435 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;55'19"N 84&deg;40'14"W</div>
        <div class="text address"><i>approx.</i> 7494, Church Road, Riley, Riley Township, Clinton County, Michigan, 48879, United States</div>
        <div class="text gop precinct">Precinct Margin: R+27%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-436">Voter 436's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 436 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;22'24"N 94&deg;20'59"W</div>
        <div class="text address"><i>approx.</i> 2526, FM 1293, Kountze, Hardin County, Texas, 77625, United States</div>
        <div class="text gop precinct">Precinct Margin: R+88%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.92210701026042,-84.67077319071407&ll=42.92210701026042,-84.67077319071407&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/434.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.37350466221498,-94.34974564878839&ll=30.37350466221498,-94.34974564878839&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/435.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-437">Voter 437's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 437 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;5'12"N 115&deg;18'16"W</div>
        <div class="text address"><i>approx.</i> 9718, Maple Sugar Leaf Place, Spring Valley, Clark County, Nevada, 89148, United States</div>
        <div class="text dem precinct">Precinct Margin: D+21%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-438">Voter 438's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 438 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;24'49"N 76&deg;33'50"W</div>
        <div class="text address"><i>approx.</i> Fiddlers Green Road, Gloucester County, Virginia, 23061, United States</div>
        <div class="text gop precinct">Precinct Margin: R+35%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.08673,-115.304559&ll=36.08673,-115.304559&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/436.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.413679,-76.563933&ll=37.413679,-76.563933&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/437.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-439">Voter 439's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 439 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;48'27"N 75&deg;36'52"W</div>
        <div class="text address"><i>approx.</i> 291, Harvard Avenue, Palmerton, Carbon County, Pennsylvania, 18071, United States</div>
        <div class="text gop precinct">Precinct Margin: R+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-440">Voter 440's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 440 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;59'45"N 82&deg;22'18"W</div>
        <div class="text address"><i>approx.</i> 337, Virginia Road, Travelers Rest, Greenville County, South Carolina, 29690, United States</div>
        <div class="text gop precinct">Precinct Margin: R+57%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.80774580456631,-75.61446803294955&ll=40.80774580456631,-75.61446803294955&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/438.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.99588525,-82.3718156003483&ll=34.99588525,-82.3718156003483&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/439.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-441">Voter 441's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 441 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;27'32"N 79&deg;59'59"W</div>
        <div class="text address"><i>approx.</i> 1408, Howard Street, East Allegheny, Pittsburgh, Allegheny County, Pennsylvania, 15212, United States</div>
        <div class="text dem precinct">Precinct Margin: D+29%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-442">Voter 442's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 442 voted for Trump</div> -->
        <div class="text coordinate"> 31&deg;27'53"N 91&deg;20'15"W</div>
        <div class="text address"><i>approx.</i> 12, McCalip-Retirement Pltn Road, Adams County, Mississippi, 39120, United States</div>
        <div class="text gop precinct">Precinct Margin: R+68%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.45906858109924,-79.9998391429912&ll=40.45906858109924,-79.9998391429912&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/440.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=31.464752438558733,-91.33768452177729&ll=31.464752438558733,-91.33768452177729&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/441.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-443">Voter 443's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 443 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;22'31"N 88&deg;44'21"W</div>
        <div class="text address"><i>approx.</i> 5612, Semmes Road, Complete, Meridian, Lauderdale County, Mississippi, 39307, United States</div>
        <div class="text dem precinct">Precinct Margin: D+63%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-444">Voter 444's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 444 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;6'56"N 79&deg;17'49"W</div>
        <div class="text address"><i>approx.</i> Olivet Cemetary Road, Marion County, South Carolina, 29589, United States</div>
        <div class="text dem precinct">Precinct Margin: D+34%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.375435759126894,-88.73918517343037&ll=32.375435759126894,-88.73918517343037&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/442.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.11569255424388,-79.29704096980568&ll=34.11569255424388,-79.29704096980568&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/443.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-445">Voter 445's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 445 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;52'31"N 73&deg;34'4"W</div>
        <div class="text address"><i>approx.</i> 445, Cascade Mountain Road, Town of Amenia, Dutchess County, New York, 12501, United States</div>
        <div class="text dem precinct">Precinct Margin: D+2%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-446">Voter 446's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 446 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;19'57"N 87&deg;6'17"W</div>
        <div class="text address"><i>approx.</i> 6128, Blue Creek Road, Valley Creek, Hueytown, Jefferson County, Alabama, 35444, United States</div>
        <div class="text gop precinct">Precinct Margin: R+32%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.8754044,-73.5679988&ll=41.8754044,-73.5679988&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/444.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.33261169451444,-87.10477896303544&ll=33.33261169451444,-87.10477896303544&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/445.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-447">Voter 447's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 447 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;28'6"N 122&deg;14'17"W</div>
        <div class="text address"><i>approx.</i> 1118, Valota Road, Redwood City, San Mateo County, California, 94061, United States</div>
        <div class="text dem precinct">Precinct Margin: D+64%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-448">Voter 448's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 448 voted for Trump</div> -->
        <div class="text coordinate"> 27&deg;17'12"N 80&deg;24'4"W</div>
        <div class="text address"><i>approx.</i> 1749, Southwest Commerce Avenue, Port Saint Lucie, Saint Lucie County, Florida, 34953, United States</div>
        <div class="text dem precinct">Precinct Margin: D+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.46853745,-122.23811624311912&ll=37.46853745,-122.23811624311912&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/446.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=27.286841,-80.40135129646123&ll=27.286841,-80.40135129646123&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/447.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-449">Voter 449's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 449 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;42'18"N 112&deg;5'57"W</div>
        <div class="text address"><i>approx.</i> Cyprus High School, Spencer Avenue, Magna, Salt Lake County, Utah, 84044, United States</div>
        <div class="text dem precinct">Precinct Margin: D+5%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-450">Voter 450's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 450 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;58'45"N 86&deg;32'44"W</div>
        <div class="text address"><i>approx.</i> 461, Cedar View Lane, Lawrence County, Indiana, 47421, United States</div>
        <div class="text gop precinct">Precinct Margin: R+52%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.70527655,-112.09944267989397&ll=40.70527655,-112.09944267989397&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/448.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.9792342,-86.5456704&ll=38.9792342,-86.5456704&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/449.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-451">Voter 451's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 451 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;41'5"N 86&deg;17'6"W</div>
        <div class="text address"><i>approx.</i> 2489, Westmoor Street, South Bend, Saint Joseph County, Indiana, 46628, United States</div>
        <div class="text dem precinct">Precinct Margin: D+73%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-452">Voter 452's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 452 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;20'29"N 86&deg;46'29"W</div>
        <div class="text address"><i>approx.</i> 6825, Wyandotte Road, Tippecanoe County, Indiana, 47905, United States</div>
        <div class="text gop precinct">Precinct Margin: R+44%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.68481041935484,-86.28518910752689&ll=41.68481041935484,-86.28518910752689&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/450.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.341461145023196,-86.77486576702383&ll=40.341461145023196,-86.77486576702383&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/451.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-453">Voter 453's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 453 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;2'29"N 107&deg;31'26"W</div>
        <div class="text address"><i>approx.</i> Indian Service Route 24, Seama, Cibola County, New Mexico, 87007, United States</div>
        <div class="text dem precinct">Precinct Margin: D+66%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-454">Voter 454's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 454 voted for Trump</div> -->
        <div class="text coordinate"> 26&deg;49'40"N 80&deg;5'23"W</div>
        <div class="text address"><i>approx.</i> 1853, Giralda Circle West, Palm Beach Gardens, North Palm Beach, Palm Beach County, Florida, 33410, United States</div>
        <div class="text gop precinct">Precinct Margin: R+5%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.04161682048902,-107.52392533266315&ll=35.04161682048902,-107.52392533266315&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/452.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.828000212730974,-80.08982205403366&ll=26.828000212730974,-80.08982205403366&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/453.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-455">Voter 455's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 455 voted for Biden</div> -->
        <div class="text coordinate"> 43&deg;42'19"N 114&deg;20'16"W</div>
        <div class="text address"><i>approx.</i> Trail Creek Golf Course, Prospect Hill Loop, Sun Valley, Blaine County, Idaho, 83353, United States</div>
        <div class="text dem precinct">Precinct Margin: D+33%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-456">Voter 456's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 456 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;8'24"N 72&deg;21'1"W</div>
        <div class="text address"><i>approx.</i> 152, Hovey Road, Monson, Hampden County, Massachusetts, 01069, United States</div>
        <div class="text gop precinct">Precinct Margin: R+5%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.70547285000001,-114.33792640435654&ll=43.70547285000001,-114.33792640435654&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/454.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.140256334387544,-72.35043669973372&ll=42.140256334387544,-72.35043669973372&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/455.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-457">Voter 457's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 457 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;30'43"N 88&deg;9'9"W</div>
        <div class="text address"><i>approx.</i> Illinois Youth Correction Center, McDonough Street, Joliet, Will County, Illinois, 60435, United States</div>
        <div class="text dem precinct">Precinct Margin: D+44%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-458">Voter 458's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 458 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;33'27"N 83&deg;47'33"W</div>
        <div class="text address"><i>approx.</i> Woodland Lake Motel, 8029, West Grand River Avenue, Brighton Township, Livingston County, Michigan, 48114, United States</div>
        <div class="text gop precinct">Precinct Margin: R+27%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.5120965,-88.15258333685657&ll=41.5120965,-88.15258333685657&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/456.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.55752975,-83.79259979610674&ll=42.55752975,-83.79259979610674&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/457.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-459">Voter 459's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 459 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;1'43"N 82&deg;0'31"W</div>
        <div class="text address"><i>approx.</i> 9073, Southeast 130th Loop, Marion County, Florida, 34491, United States</div>
        <div class="text gop precinct">Precinct Margin: R+27%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-460">Voter 460's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 460 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;7'55"N 81&deg;39'39"W</div>
        <div class="text address"><i>approx.</i> 199, Lethbridge Circle, Stoney Hill, Copley Township, Summit County, Ohio, 44321, United States</div>
        <div class="text dem precinct">Precinct Margin: D+15%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.028643922655505,-82.0088538185542&ll=29.028643922655505,-82.0088538185542&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/458.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.132159,-81.661008&ll=41.132159,-81.661008&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/459.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-461">Voter 461's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 461 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;7'9"N 75&deg;7'55"W</div>
        <div class="text address"><i>approx.</i> 1081, Edge Hill Road, Mount Vernon Gardens, Abington, Abington Township, Montgomery County, Pennsylvania, 19001, United States</div>
        <div class="text dem precinct">Precinct Margin: D+42%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-462">Voter 462's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 462 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;37'27"N 94&deg;28'36"W</div>
        <div class="text address"><i>approx.</i> 1781, Hobbs Road, McDonald County, Missouri, 64831, United States</div>
        <div class="text gop precinct">Precinct Margin: R+69%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.11926026745004,-75.13205185224334&ll=40.11926026745004,-75.13205185224334&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/460.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.624274524426724,-94.47670554826766&ll=36.624274524426724,-94.47670554826766&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/461.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-463">Voter 463's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 463 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;0'54"N 95&deg;46'0"W</div>
        <div class="text address"><i>approx.</i> 6220, Southwest 29th Street, Topeka, Shawnee County, Kansas, 66614, United States</div>
        <div class="text dem precinct">Precinct Margin: D+40%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-464">Voter 464's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 464 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;34'59"N 124&deg;5'8"W</div>
        <div class="text address"><i>approx.</i> 4000 Road, Coos County, Oregon, 97449, United States</div>
        <div class="text gop precinct">Precinct Margin: R+32%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.01501685,-95.76670190049279&ll=39.01501685,-95.76670190049279&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/462.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.58319202103655,-124.08563734625642&ll=43.58319202103655,-124.08563734625642&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/463.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-465">Voter 465's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 465 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;5'23"N 117&deg;56'6"W</div>
        <div class="text address"><i>approx.</i> San Bernardino Road, West Covina, Los Angeles County, California, 91722, United States</div>
        <div class="text dem precinct">Precinct Margin: D+35%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-466">Voter 466's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 466 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;5'19"N 118&deg;17'48"W</div>
        <div class="text address"><i>approx.</i> 954, North Kenmore Avenue, Bicycle District, East Hollywood, Los Angeles, Los Angeles County, California, 90029, United States</div>
        <div class="text dem precinct">Precinct Margin: D+53%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.08999933939972,-117.93509379152432&ll=34.08999933939972,-117.93509379152432&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/464.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.088840727272725,-118.29692045454546&ll=34.088840727272725,-118.29692045454546&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/465.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-467">Voter 467's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 467 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;54'42"N 73&deg;54'2"W</div>
        <div class="text address"><i>approx.</i> 6154, Liebig Avenue, Bronx County, The Bronx, City of New York, New York, 10471, United States</div>
        <div class="text dem precinct">Precinct Margin: D+41%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-468">Voter 468's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 468 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;33'34"N 89&deg;5'36"W</div>
        <div class="text address"><i>approx.</i> 333, Sessions Road, Newton County, Mississippi, 39365, United States</div>
        <div class="text gop precinct">Precinct Margin: R+76%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.9116702,-73.90064194565133&ll=40.9116702,-73.90064194565133&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/466.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.559708442837795,-89.09343430108449&ll=32.559708442837795,-89.09343430108449&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/467.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-469">Voter 469's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 469 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;39'51"N 97&deg;28'17"W</div>
        <div class="text address"><i>approx.</i> Winscott Road, Benbrook, Tarrant County, Texas, 76126, United States</div>
        <div class="text gop precinct">Precinct Margin: R+28%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-470">Voter 470's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 470 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;4'45"N 118&deg;7'53"W</div>
        <div class="text address"><i>approx.</i> The Church of Jesus Christ of Latter-day Saints, Shorb Street, Alhambra, Los Angeles County, California, 91801, United States</div>
        <div class="text dem precinct">Precinct Margin: D+39%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.664408821857855,-97.47165728956064&ll=32.664408821857855,-97.47165728956064&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/468.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.0791766,-118.1314591&ll=34.0791766,-118.1314591&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/469.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-471">Voter 471's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 471 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;55'24"N 76&deg;15'47"W</div>
        <div class="text address"><i>approx.</i> 8059, West Glen Road, Denby Park, Monticello Village, Norfolk, Virginia, 23505, United States</div>
        <div class="text dem precinct">Precinct Margin: D+45%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-472">Voter 472's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 472 voted for Trump</div> -->
        <div class="text coordinate"> 47&deg;2'49"N 122&deg;50'13"W</div>
        <div class="text address"><i>approx.</i> Dirty Dave's Pizza Parlor, Martin Way East, Olympia, Thurston County, Washington, 98506-5049, United States</div>
        <div class="text dem precinct">Precinct Margin: D+26%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.92348405,-76.26315648534816&ll=36.92348405,-76.26315648534816&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/470.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.0471069,-122.8370478&ll=47.0471069,-122.8370478&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/471.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-473">Voter 473's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 473 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;41'17"N 87&deg;47'36"W</div>
        <div class="text address"><i>approx.</i> 11185, South Worth Avenue, Worth, Worth Township, Cook County, Illinois, 60482, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-474">Voter 474's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 474 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;11'17"N 112&deg;20'20"W</div>
        <div class="text address"><i>approx.</i> 267, North Stout Avenue, Blackfoot, Bingham County, Idaho, 83221, United States</div>
        <div class="text gop precinct">Precinct Margin: R+58%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.688194469085886,-87.79352948610887&ll=41.688194469085886,-87.79352948610887&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/472.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.18815077454186,-112.3389034724753&ll=43.18815077454186,-112.3389034724753&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/473.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-475">Voter 475's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 475 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;55'57"N 118&deg;0'35"W</div>
        <div class="text address"><i>approx.</i> 15199, Fernview Street, Los Angeles County, California, 90604, United States</div>
        <div class="text dem precinct">Precinct Margin: D+21%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-476">Voter 476's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 476 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;25'7"N 92&deg;48'40"W</div>
        <div class="text address"><i>approx.</i> Letterman Cemetery, Bowen Creek Road, Webster County, Missouri, 65713, United States</div>
        <div class="text gop precinct">Precinct Margin: R+68%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.932752,-118.009988&ll=33.932752,-118.009988&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/474.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.4186571,-92.8112861&ll=37.4186571,-92.8112861&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/475.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-477">Voter 477's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 477 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;35'45"N 124&deg;8'53"W</div>
        <div class="text address"><i>approx.</i> 1700, Stillman Way, Fortuna, Humboldt County, California, 95540, United States</div>
        <div class="text gop precinct">Precinct Margin: R+6%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-478">Voter 478's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 478 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;58'34"N 83&deg;47'22"W</div>
        <div class="text address"><i>approx.</i> Klopfer Road, Bolingbroke, Monroe County, Georgia, 31004, United States</div>
        <div class="text gop precinct">Precinct Margin: R+56%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.596003,-124.148308&ll=40.596003,-124.148308&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/476.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.976292870197,-83.7896726686569&ll=32.976292870197,-83.7896726686569&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/477.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-479">Voter 479's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 479 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;12'33"N 97&deg;48'5"W</div>
        <div class="text address"><i>approx.</i> 1911, Cannonwood Lane, Cherry Creek, Austin, Travis County, Texas, 78745, United States</div>
        <div class="text dem precinct">Precinct Margin: D+65%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-480">Voter 480's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 480 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;43'51"N 74&deg;19'53"W</div>
        <div class="text address"><i>approx.</i> Micaja S Road, Stafford Township, Ocean County, New Jersey, 08050-2808, United States</div>
        <div class="text gop precinct">Precinct Margin: R+25%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.2093875,-97.8015889795828&ll=30.2093875,-97.8015889795828&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/478.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.73096137133364,-74.331606233302&ll=39.73096137133364,-74.331606233302&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/479.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-481">Voter 481's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 481 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;50'5"N 71&deg;24'12"W</div>
        <div class="text address"><i>approx.</i> Hope High School, 324, Hope Street, College Hill, Providence, Providence County, Rhode Island, 02906, United States</div>
        <div class="text dem precinct">Precinct Margin: D+80%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-482">Voter 482's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 482 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;2'42"N 81&deg;1'51"W</div>
        <div class="text address"><i>approx.</i> 2095, Driftwood Circle, York County, South Carolina, 29708, United States</div>
        <div class="text gop precinct">Precinct Margin: R+13%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.83493875,-71.40343134409326&ll=41.83493875,-71.40343134409326&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/480.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.04500028078476,-81.03096294268862&ll=35.04500028078476,-81.03096294268862&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/481.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-483">Voter 483's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 483 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;28'0"N 84&deg;17'23"W</div>
        <div class="text address"><i>approx.</i> 7523, Botkins Road, Botkins, Shelby County, Ohio, 45306, United States</div>
        <div class="text gop precinct">Precinct Margin: R+76%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-484">Voter 484's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 484 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;3'51"N 119&deg;45'33"W</div>
        <div class="text address"><i>approx.</i> Heybourne Road, Johnson Lane, Douglas County, Nevada, 89705, United States</div>
        <div class="text gop precinct">Precinct Margin: R+36%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.466770328406696,-84.28985574835154&ll=40.466770328406696,-84.28985574835154&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/482.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.064377629544936,-119.75929164723927&ll=39.064377629544936,-119.75929164723927&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/483.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-485">Voter 485's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 485 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;37'9"N 82&deg;32'26"W</div>
        <div class="text address"><i>approx.</i> 927, Columbia Street, Pointe aux Chenes, Algonac, Saint Clair County, Michigan, 48001, United States</div>
        <div class="text gop precinct">Precinct Margin: R+37%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-486">Voter 486's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 486 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;42'27"N 80&deg;17'6"W</div>
        <div class="text address"><i>approx.</i> 498, Ohio Avenue, Rochester, Beaver County, Pennsylvania, 15074, United States</div>
        <div class="text dem precinct">Precinct Margin: D+10%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.61938691836735,-82.54059963265307&ll=42.61938691836735,-82.54059963265307&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/484.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.707719,-80.285048&ll=40.707719,-80.285048&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/485.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-487">Voter 487's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 487 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;5'39"N 94&deg;6'42"W</div>
        <div class="text address"><i>approx.</i> 10140, 40th Street Southwest, Middleville Township, Wright County, Minnesota, 55349, United States</div>
        <div class="text gop precinct">Precinct Margin: R+56%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-488">Voter 488's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 488 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;8'58"N 96&deg;7'58"W</div>
        <div class="text address"><i>approx.</i> 129th West Avenue, Sand Springs, Tulsa County, Oklahoma, 74063, United States</div>
        <div class="text gop precinct">Precinct Margin: R+43%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.094242216530375,-94.11176089881272&ll=45.094242216530375,-94.11176089881272&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/486.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.149452354669606,-96.13280577494079&ll=36.149452354669606,-96.13280577494079&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/487.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-489">Voter 489's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 489 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;22'23"N 76&deg;41'58"W</div>
        <div class="text address"><i>approx.</i> 3117, Northbrook Road, Wellwood, Pikesville, Baltimore County, Maryland, 21208, United States</div>
        <div class="text gop precinct">Precinct Margin: R+11%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-490">Voter 490's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 490 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;11'50"N 82&deg;17'37"W</div>
        <div class="text address"><i>approx.</i> Hogback Mountain Road, Greenville County, South Carolina, United States</div>
        <div class="text dem precinct">Precinct Margin: D+5%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.373195749999994,-76.69958115622549&ll=39.373195749999994,-76.69958115622549&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/488.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.1974453,-82.29383&ll=35.1974453,-82.29383&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/489.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-491">Voter 491's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 491 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;12'15"N 96&deg;57'0"W</div>
        <div class="text address"><i>approx.</i> 809, Lake Forest Trail, Denton County, Texas, 75068, United States</div>
        <div class="text dem precinct">Precinct Margin: D+13%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-492">Voter 492's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 492 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;44'41"N 97&deg;13'35"W</div>
        <div class="text address"><i>approx.</i> 2329, Pollard Street, Fort Worth, Tarrant County, Texas, 76112, United States</div>
        <div class="text dem precinct">Precinct Margin: D+66%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.204415837581784,-96.95010115302773&ll=33.204415837581784,-96.95010115302773&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/490.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.74485095,-97.22643578799298&ll=32.74485095,-97.22643578799298&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/491.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-493">Voter 493's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 493 voted for Biden</div> -->
        <div class="text coordinate"> 47&deg;40'20"N 122&deg;15'57"W</div>
        <div class="text address"><i>approx.</i> 6008, 57th Avenue Northeast, Windermere, Seattle, King County, Washington, 98115, United States</div>
        <div class="text dem precinct">Precinct Margin: D+83%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-494">Voter 494's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 494 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;31'38"N 116&deg;5'4"W</div>
        <div class="text address"><i>approx.</i> BLM Track, Ada County, Idaho, 83715, United States</div>
        <div class="text dem precinct">Precinct Margin: D+12%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.67242315,-122.26585052089737&ll=47.67242315,-122.26585052089737&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/492.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.527444,-116.0846046&ll=43.527444,-116.0846046&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/493.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-495">Voter 495's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 495 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;13'9"N 81&deg;32'24"W</div>
        <div class="text address"><i>approx.</i> Lovelace Family Medicine, P.A, 600, North Wheeler Ave, Prosperity, Newberry County, South Carolina, 29127, United States</div>
        <div class="text gop precinct">Precinct Margin: R+10%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-496">Voter 496's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 496 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;37'5"N 80&deg;13'39"W</div>
        <div class="text address"><i>approx.</i> 440, New Hope Church Road, Patrick County, Virginia, 24171, United States</div>
        <div class="text gop precinct">Precinct Margin: R+56%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.219218,-81.5401978&ll=34.219218,-81.5401978&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/494.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.618275,-80.227628&ll=36.618275,-80.227628&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/495.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-497">Voter 497's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 497 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;38'56"N 112&deg;3'46"W</div>
        <div class="text address"><i>approx.</i> Bona Venture Mobile Home Park, East Grovers Avenue, Quail Country Place, Phoenix, Maricopa County, Arizona, 85023-1501, United States</div>
        <div class="text dem precinct">Precinct Margin: D+2%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-498">Voter 498's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 498 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;47'28"N 122&deg;33'18"W</div>
        <div class="text address"><i>approx.</i> 1702, Northwest 14th Circle, Battle Ground, Clark County, Washington, 98604, United States</div>
        <div class="text gop precinct">Precinct Margin: R+26%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.6489934,-112.06305205000001&ll=33.6489934,-112.06305205000001&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/496.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.7911522,-122.55518223366363&ll=45.7911522,-122.55518223366363&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/497.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-499">Voter 499's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 499 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;48'44"N 87&deg;41'56"W</div>
        <div class="text address"><i>approx.</i> 4445, South Sacramento Avenue, Brighton Park, Chicago, Lake Township, Cook County, Illinois, 60632, United States</div>
        <div class="text dem precinct">Precinct Margin: D+64%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-500">Voter 500's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 500 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;30'9"N 78&deg;25'3"W</div>
        <div class="text address"><i>approx.</i> 944, 28th Street, Westmont, Altoona, Blair County, Pennsylvania, 16601, United States</div>
        <div class="text gop precinct">Precinct Margin: R+22%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.81235425,-87.69897255000001&ll=41.81235425,-87.69897255000001&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/498.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.502665202368064,-78.41769783281819&ll=40.502665202368064,-78.41769783281819&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/499.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-501">Voter 501's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 501 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;9'29"N 118&deg;22'29"W</div>
        <div class="text address"><i>approx.</i> 11210, La Maida Street, Los Angeles, Los Angeles County, California, 91601, United States</div>
        <div class="text dem precinct">Precinct Margin: D+76%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-502">Voter 502's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 502 voted for Trump</div> -->
        <div class="text coordinate"> 47&deg;56'29"N 122&deg;14'58"W</div>
        <div class="text address"><i>approx.</i> Industry Street, Everett, Snohomish County, Washington, 98203, United States</div>
        <div class="text dem precinct">Precinct Margin: D+26%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.15832873684211,-118.37493684210527&ll=34.15832873684211,-118.37493684210527&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/500.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.94161989657705,-122.24958146637024&ll=47.94161989657705,-122.24958146637024&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/501.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-503">Voter 503's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 503 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;29'2"N 90&deg;1'22"W</div>
        <div class="text address"><i>approx.</i> Mine Haul Road, Belleville, Saint Clair County, Illinois, 62220, United States</div>
        <div class="text gop precinct">Precinct Margin: R+14%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-504">Voter 504's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 504 voted for Trump</div> -->
        <div class="text coordinate"> 46&deg;41'32"N 95&deg;52'5"W</div>
        <div class="text address"><i>approx.</i> 50342, Trowbridge Circle, Otter Tail County, Minnesota, 56587, United States</div>
        <div class="text gop precinct">Precinct Margin: R+45%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.483910655499585,-90.02286172324531&ll=38.483910655499585,-90.02286172324531&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/502.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=46.692276990020794,-95.86825624400188&ll=46.692276990020794,-95.86825624400188&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/503.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-505">Voter 505's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 505 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;42'48"N 95&deg;47'52"W</div>
        <div class="text address"><i>approx.</i> 8753, Willowleaf Garden Crossing, Seven Meadows, Fort Bend County, Texas, 77494, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-506">Voter 506's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 506 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;36'32"N 80&deg;19'35"W</div>
        <div class="text address"><i>approx.</i> 7793, Russell Creek Road, Charleston County, South Carolina, 29438, United States</div>
        <div class="text dem precinct">Precinct Margin: D+3%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.713491142857144,-95.79781410204082&ll=29.713491142857144,-95.79781410204082&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/504.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.60896,-80.326588&ll=32.60896,-80.326588&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/505.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-507">Voter 507's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 507 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;50'16"N 97&deg;39'52"W</div>
        <div class="text address"><i>approx.</i> 235, Appaloosa Cove, Williamson County, Texas, 76527, United States</div>
        <div class="text gop precinct">Precinct Margin: R+31%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-508">Voter 508's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 508 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;42'8"N 91&deg;22'51"W</div>
        <div class="text address"><i>approx.</i> 2311, Chestnut Street, Hannibal, Marion County, Missouri, 63401, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.837949811759124,-97.6644791961863&ll=30.837949811759124,-97.6644791961863&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/506.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.702467921029736,-91.38084565290768&ll=39.702467921029736,-91.38084565290768&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/507.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-509">Voter 509's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 509 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;21'59"N 76&deg;40'41"W</div>
        <div class="text address"><i>approx.</i> 436, Perkins Road, Carter, Gates County, North Carolina, 27938, United States</div>
        <div class="text gop precinct">Precinct Margin: R+34%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-510">Voter 510's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 510 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;47'3"N 88&deg;2'57"W</div>
        <div class="text address"><i>approx.</i> 5605, Katrine Avenue, Woodridge, DuPage County, Illinois, 60516, United States</div>
        <div class="text dem precinct">Precinct Margin: D+32%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.366662679662234,-76.67809148640013&ll=36.366662679662234,-76.67809148640013&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/508.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.784285,-88.04928728571429&ll=41.784285,-88.04928728571429&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/509.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-511">Voter 511's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 511 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;15'42"N 77&deg;13'59"W</div>
        <div class="text address"><i>approx.</i> 1770, Longs Gap Road, Cumberland County, Pennsylvania, 17013, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-512">Voter 512's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 512 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;24'28"N 97&deg;8'46"W</div>
        <div class="text address"><i>approx.</i> 3989, Huling Road, Denton County, Texas, 76266, United States</div>
        <div class="text gop precinct">Precinct Margin: R+58%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.261913923637714,-77.23306534270348&ll=40.261913923637714,-77.23306534270348&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/510.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.40801001270326,-97.14613415029244&ll=33.40801001270326,-97.14613415029244&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/511.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-513">Voter 513's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 513 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;13'8"N 93&deg;15'38"W</div>
        <div class="text address"><i>approx.</i> 414, Columbia Rd 5, Horsehead, Columbia County, Arkansas, 71753, United States</div>
        <div class="text gop precinct">Precinct Margin: R+27%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-514">Voter 514's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 514 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;53'7"N 118&deg;7'10"W</div>
        <div class="text address"><i>approx.</i> 16330, Cornuta Avenue, Bellflower, Los Angeles County, California, 90706, United States</div>
        <div class="text dem precinct">Precinct Margin: D+31%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.218908489748955,-93.26077014254453&ll=33.218908489748955,-93.26077014254453&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/512.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.88535,-118.119708&ll=33.88535,-118.119708&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/513.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-515">Voter 515's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 515 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;35'58"N 70&deg;56'38"W</div>
        <div class="text address"><i>approx.</i> 32, Atlantic Street, Redgate Corner, Dartmouth, Bristol County, Massachusetts, 02748, United States</div>
        <div class="text dem precinct">Precinct Margin: D+27%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-516">Voter 516's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 516 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;35'54"N 83&deg;51'30"W</div>
        <div class="text address"><i>approx.</i> 1157, Elm Street Northeast, Covington, Newton County, Georgia, 30014, United States</div>
        <div class="text gop precinct">Precinct Margin: R+1%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.59969135,-70.94393059263942&ll=41.59969135,-70.94393059263942&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/514.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.598537,-83.858607&ll=33.598537,-83.858607&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/515.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-517">Voter 517's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 517 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;41'38"N 88&deg;45'26"W</div>
        <div class="text address"><i>approx.</i> Siloam-Una Road, Clay County, Mississippi, United States</div>
        <div class="text dem precinct">Precinct Margin: D+53%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-518">Voter 518's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 518 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;24'46"N 118&deg;32'27"W</div>
        <div class="text address"><i>approx.</i> The Original Saugus Cafe, 25861, Railroad Avenue, Pardee, Santa Clarita, Los Angeles County, California, 91355, United States</div>
        <div class="text dem precinct">Precinct Margin: D+1%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.69406447006862,-88.75743394666848&ll=33.69406447006862,-88.75743394666848&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/516.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.41286690247964,-118.54097011761182&ll=34.41286690247964,-118.54097011761182&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/517.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-519">Voter 519's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 519 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;10'34"N 79&deg;53'44"W</div>
        <div class="text address"><i>approx.</i> 1098, Whitehall Shores Road, Heritage, Florence County, South Carolina, 29501, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-520">Voter 520's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 520 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;25'49"N 111&deg;50'49"W</div>
        <div class="text address"><i>approx.</i> Townhomes, East Crosscourt Way, Lehi, Utah County, Utah, United States</div>
        <div class="text gop precinct">Precinct Margin: R+56%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.17612547397282,-79.89561814680417&ll=34.17612547397282,-79.89561814680417&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/518.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.43029305,-111.84698205000001&ll=40.43029305,-111.84698205000001&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/519.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-521">Voter 521's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 521 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;43'13"N 118&deg;2'47"W</div>
        <div class="text address"><i>approx.</i> 16642, Blanton Street, Huntington Beach, Orange County, California, 92649, United States</div>
        <div class="text dem precinct">Precinct Margin: D+8%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-522">Voter 522's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 522 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;38'8"N 74&deg;20'21"W</div>
        <div class="text address"><i>approx.</i> 180, Riverview Road, Town of Middleburgh, Schoharie County, New York, 12157, United States</div>
        <div class="text gop precinct">Precinct Margin: R+42%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.72039875,-118.04663499995742&ll=33.72039875,-118.04663499995742&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/520.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.6356538,-74.3392038&ll=42.6356538,-74.3392038&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/521.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-523">Voter 523's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 523 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;24'0"N 122&deg;47'17"W</div>
        <div class="text address"><i>approx.</i> 16565, Southwest 108th Avenue, Cook Park, Tigard, Washington County, Oregon, 97224, United States</div>
        <div class="text dem precinct">Precinct Margin: D+36%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-524">Voter 524's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 524 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;32'24"N 94&deg;33'50"W</div>
        <div class="text address"><i>approx.</i> 32439, Nelson Road, Cass County, Missouri, 64742, United States</div>
        <div class="text gop precinct">Precinct Margin: R+56%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.4002074,-122.78827967659623&ll=45.4002074,-122.78827967659623&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/522.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.54021813575871,-94.56395944092188&ll=38.54021813575871,-94.56395944092188&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/523.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-525">Voter 525's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 525 voted for Biden</div> -->
        <div class="text coordinate"> 27&deg;41'39"N 97&deg;24'38"W</div>
        <div class="text address"><i>approx.</i> 4130, Walnut Hills Drive, Corpus Christi, Nueces County, Texas, 78413, United States</div>
        <div class="text dem precinct">Precinct Margin: D+4%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-526">Voter 526's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 526 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;17'31"N 78&deg;16'29"W</div>
        <div class="text address"><i>approx.</i> 77, Bethel Lane, Rochelle, Madison County, Virginia, 22738, United States</div>
        <div class="text gop precinct">Precinct Margin: R+30%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=27.694279328241244,-97.41065345863005&ll=27.694279328241244,-97.41065345863005&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/524.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.29211822660646,-78.27489822660645&ll=38.29211822660646,-78.27489822660645&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/525.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-527">Voter 527's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 527 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;16'41"N 73&deg;55'58"W</div>
        <div class="text address"><i>approx.</i> Louisa Street, City of Peekskill, Westchester County, New York, 10566, United States</div>
        <div class="text dem precinct">Precinct Margin: D+36%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-528">Voter 528's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 528 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;41'28"N 86&deg;52'6"W</div>
        <div class="text address"><i>approx.</i> 1722, Welnetz Road, Michigan City, LaPorte County, Indiana, 46360, United States</div>
        <div class="text dem precinct">Precinct Margin: D+48%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.278240239112606,-73.93282642573159&ll=41.278240239112606,-73.93282642573159&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/526.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.69118269693151,-86.86833762745646&ll=41.69118269693151,-86.86833762745646&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/527.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-529">Voter 529's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 529 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;41'8"N 73&deg;58'15"W</div>
        <div class="text address"><i>approx.</i> Brooklyn Bear's Community Gardens - Carlton, Carlton Avenue, Brooklyn, Kings County, City of New York, New York, 11238, United States</div>
        <div class="text dem precinct">Precinct Margin: D+95%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-530">Voter 530's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 530 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;22'6"N 81&deg;23'6"W</div>
        <div class="text address"><i>approx.</i> 408, West Old Post Road, Cherryville, Gaston County, North Carolina, 28021, United States</div>
        <div class="text gop precinct">Precinct Margin: R+47%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.6856947,-73.97104522810046&ll=40.6856947,-73.97104522810046&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/528.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.36837952130239,-81.3852568849901&ll=35.36837952130239,-81.3852568849901&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/529.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-531">Voter 531's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 531 voted for Biden</div> -->
        <div class="text coordinate"> 43&deg;57'24"N 91&deg;0'37"W</div>
        <div class="text address"><i>approx.</i> Balmer Road, Town of Burns, La Crosse County, Wisconsin, United States</div>
        <div class="text gop precinct">Precinct Margin: R+22%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-532">Voter 532's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 532 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;41'36"N 77&deg;37'39"W</div>
        <div class="text address"><i>approx.</i> 5739, Pouncey Tract Road, Henrico County, Virginia, 23059, United States</div>
        <div class="text gop precinct">Precinct Margin: R+2%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.95687,-91.01033&ll=43.95687,-91.01033&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/530.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.6933357,-77.6277749&ll=37.6933357,-77.6277749&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/531.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-533">Voter 533's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 533 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;10'12"N 87&deg;49'2"W</div>
        <div class="text address"><i>approx.</i> 1598, Grove Avenue, Briergate, Highland Park, Lake County, Illinois, 60035, United States</div>
        <div class="text dem precinct">Precinct Margin: D+61%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-534">Voter 534's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 534 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;4'25"N 96&deg;53'19"W</div>
        <div class="text address"><i>approx.</i> Tommy Briggs Cougar Stadium, Cougar Alley, The Colony, Denton County, Texas, 75056, United States</div>
        <div class="text dem precinct">Precinct Margin: D+4%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.170171888758034,-87.81725740199003&ll=42.170171888758034,-87.81725740199003&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/532.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.0738456,-96.88879796504503&ll=33.0738456,-96.88879796504503&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/533.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-535">Voter 535's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 535 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;41'33"N 111&deg;50'3"W</div>
        <div class="text address"><i>approx.</i> Main Street, Millville, Cache County, Utah, 84326, United States</div>
        <div class="text gop precinct">Precinct Margin: R+58%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-536">Voter 536's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 536 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;43'25"N 84&deg;44'6"W</div>
        <div class="text address"><i>approx.</i> Estelle Mddleton Road, Murray County, Georgia, United States</div>
        <div class="text gop precinct">Precinct Margin: R+77%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.692685121775064,-111.83439541906236&ll=41.692685121775064,-111.83439541906236&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/534.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.7236389,-84.7351784&ll=34.7236389,-84.7351784&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/535.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-537">Voter 537's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 537 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;44'25"N 117&deg;25'36"W</div>
        <div class="text address"><i>approx.</i> 13375, Temescal Canyon Road, Alberhill, Lake Elsinore, Riverside County, California, 92883, United States</div>
        <div class="text gop precinct">Precinct Margin: R+4%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-538">Voter 538's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 538 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;44'48"N 87&deg;46'4"W</div>
        <div class="text address"><i>approx.</i> 5882, 80th Street, Burbank, Stickney Township, Cook County, Illinois, 60459, United States</div>
        <div class="text dem precinct">Precinct Margin: D+4%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.74042668709042,-117.42680659807668&ll=33.74042668709042,-117.42680659807668&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/536.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.74692666081979,-87.76797057661469&ll=41.74692666081979,-87.76797057661469&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/537.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-539">Voter 539's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 539 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;6'43"N 84&deg;27'44"W</div>
        <div class="text address"><i>approx.</i> 1606, Barnes Road, Woodstock, Cherokee County, Georgia, 30188, United States</div>
        <div class="text gop precinct">Precinct Margin: R+23%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-540">Voter 540's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 540 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;37'43"N 122&deg;34'4"W</div>
        <div class="text address"><i>approx.</i> 10392, Northeast 10th Street, Marrion, Vancouver, Clark County, Washington, 98664, United States</div>
        <div class="text dem precinct">Precinct Margin: D+14%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.11202495863785,-84.46223183099686&ll=34.11202495863785,-84.46223183099686&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/538.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.628829483890165,-122.56787918316718&ll=45.628829483890165,-122.56787918316718&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/539.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-541">Voter 541's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 541 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;39'27"N 83&deg;25'26"W</div>
        <div class="text address"><i>approx.</i> Suburban Ford of Waterford, 6975, Highland Road, Waterford Township, Oakland County, Michigan, 48327, United States</div>
        <div class="text dem precinct">Precinct Margin: D+4%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-542">Voter 542's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 542 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;7'53"N 81&deg;5'54"W</div>
        <div class="text address"><i>approx.</i> 5898, Hidden Oaks Lane, York County, South Carolina, 29710, United States</div>
        <div class="text gop precinct">Precinct Margin: R+30%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.657536050000004,-83.42415324518065&ll=42.657536050000004,-83.42415324518065&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/540.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.1315334,-81.0983617&ll=35.1315334,-81.0983617&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/541.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-543">Voter 543's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 543 voted for Biden</div> -->
        <div class="text coordinate"> 25&deg;53'45"N 80&deg;11'50"W</div>
        <div class="text address"><i>approx.</i> 90, Northeast 132nd Street, North Miami, Miami-Dade County, Florida, 33161, United States</div>
        <div class="text dem precinct">Precinct Margin: D+75%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-544">Voter 544's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 544 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;29'9"N 90&deg;40'5"W</div>
        <div class="text address"><i>approx.</i> 201, River Ridge Street, South End District, Dubuque, Dubuque County, Iowa, 52003, United States</div>
        <div class="text dem precinct">Precinct Margin: D+18%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=25.8959266,-80.19728791796823&ll=25.8959266,-80.19728791796823&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/542.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.485965,-90.66818&ll=42.485965,-90.66818&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/543.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-545">Voter 545's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 545 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;58'33"N 122&deg;29'9"W</div>
        <div class="text address"><i>approx.</i> 47, Lochinvar Road, San Rafael, Marin County, California, 94901, United States</div>
        <div class="text dem precinct">Precinct Margin: D+56%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-546">Voter 546's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 546 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;24'27"N 93&deg;28'39"W</div>
        <div class="text address"><i>approx.</i> Tank Road, Calcasieu Parish, Louisiana, 70633, United States</div>
        <div class="text gop precinct">Precinct Margin: R+67%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.9758577249888,-122.48607622802616&ll=37.9758577249888,-122.48607622802616&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/544.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.407547956227557,-93.47752154715698&ll=30.407547956227557,-93.47752154715698&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/545.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-547">Voter 547's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 547 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;18'22"N 71&deg;19'29"W</div>
        <div class="text address"><i>approx.</i> Crosstown Trail and Cochituate Aqueduct Trail, Overbrook, Wellesley, Norfolk County, Massachusetts, 01500, United States</div>
        <div class="text dem precinct">Precinct Margin: D+60%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-548">Voter 548's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 548 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;51'40"N 96&deg;2'10"W</div>
        <div class="text address"><i>approx.</i> 13652, Pioneer Avenue, Plymouth County, Iowa, 51031, United States</div>
        <div class="text gop precinct">Precinct Margin: R+61%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.30618975365207,-71.3248658721171&ll=42.30618975365207,-71.3248658721171&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/546.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.861243659110905,-96.03615413365573&ll=42.861243659110905,-96.03615413365573&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/547.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-549">Voter 549's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 549 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;44'45"N 75&deg;19'50"W</div>
        <div class="text address"><i>approx.</i> 115, Delaware Crossing, Woolwich Township, Gloucester County, New Jersey, 08085, United States</div>
        <div class="text dem precinct">Precinct Margin: D+3%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-550">Voter 550's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 550 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;39'5"N 95&deg;33'20"W</div>
        <div class="text address"><i>approx.</i> 11696, Dover Street, Houston, Harris County, Texas, 77031, United States</div>
        <div class="text dem precinct">Precinct Margin: D+33%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.74610569222371,-75.33066622754477&ll=39.74610569222371,-75.33066622754477&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/548.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.651563663316583,-95.55561967839196&ll=29.651563663316583,-95.55561967839196&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/549.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-551">Voter 551's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 551 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;58'14"N 93&deg;15'2"W</div>
        <div class="text address"><i>approx.</i> Brian Coyle Center, 420, 15th Avenue South, Phillips, Minneapolis, Hennepin County, Minnesota, 55454, United States</div>
        <div class="text dem precinct">Precinct Margin: D+84%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-552">Voter 552's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 552 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;47'49"N 76&deg;7'23"W</div>
        <div class="text address"><i>approx.</i> Parkway Elementary, Macarthur Road, Salem, Virginia Beach, Virginia, 23456, United States</div>
        <div class="text dem precinct">Precinct Margin: D+43%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.97062895,-93.2506277600898&ll=44.97062895,-93.2506277600898&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/550.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.7970533,-76.12325748294563&ll=36.7970533,-76.12325748294563&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/551.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-553">Voter 553's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 553 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;44'48"N 121&deg;11'39"W</div>
        <div class="text address"><i>approx.</i> 5748, Macargo Road, Placer County, California, 95746, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-554">Voter 554's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 554 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;31'41"N 86&deg;51'57"W</div>
        <div class="text address"><i>approx.</i> 2191, Mert Lane, Tippecanoe County, Indiana, 47920, United States</div>
        <div class="text gop precinct">Precinct Margin: R+26%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.746826362700475,-121.194243606415&ll=38.746826362700475,-121.194243606415&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/552.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.5280741,-86.8658982&ll=40.5280741,-86.8658982&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/553.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-555">Voter 555's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 555 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;0'22"N 83&deg;52'19"W</div>
        <div class="text address"><i>approx.</i> 129, Pelham Road, Chilhowee Hills, Knoxville, Knox County, Tennessee, 37914, United States</div>
        <div class="text dem precinct">Precinct Margin: D+65%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-556">Voter 556's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 556 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;11'49"N 118&deg;37'11"W</div>
        <div class="text address"><i>approx.</i> 22629, Marlin Place, Los Angeles, Los Angeles County, California, 91307, United States</div>
        <div class="text dem precinct">Precinct Margin: D+29%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.00615677043138,-83.87200845785229&ll=36.00615677043138,-83.87200845785229&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/554.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.196960062500004,-118.61972618749999&ll=34.196960062500004,-118.61972618749999&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/555.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-557">Voter 557's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 557 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;35'24"N 112&deg;0'52"W</div>
        <div class="text address"><i>approx.</i> 8757, Grizzly Way, West Jordan, Salt Lake County, Utah, 84081, United States</div>
        <div class="text gop precinct">Precinct Margin: R+32%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-558">Voter 558's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 558 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;48'11"N 80&deg;50'20"W</div>
        <div class="text address"><i>approx.</i> 624, Old Farm Road, Old Farm, Statesville, Iredell County, North Carolina, 28625, United States</div>
        <div class="text gop precinct">Precinct Margin: R+17%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.59013100157185,-112.01456737588465&ll=40.59013100157185,-112.01456737588465&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/556.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.8032463,-80.8389095&ll=35.8032463,-80.8389095&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/557.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-559">Voter 559's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 559 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;15'38"N 75&deg;32'44"W</div>
        <div class="text address"><i>approx.</i> 152, Morgan Hill Road, North Afton, Town of Afton, Chenango County, New York, 13730, United States</div>
        <div class="text gop precinct">Precinct Margin: R+33%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-560">Voter 560's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 560 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;24'40"N 95&deg;32'52"W</div>
        <div class="text address"><i>approx.</i> 12305, Maggie Lane, Montgomery County, Texas, 77318, United States</div>
        <div class="text gop precinct">Precinct Margin: R+68%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.2605739194193,-75.54572851355448&ll=42.2605739194193,-75.54572851355448&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/558.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.41115440844056,-95.54779619954347&ll=30.41115440844056,-95.54779619954347&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/559.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-561">Voter 561's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 561 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;50'7"N 87&deg;47'56"W</div>
        <div class="text address"><i>approx.</i> 3129, Wenonah Avenue, Berwyn, Cook County, Illinois, 60402, United States</div>
        <div class="text dem precinct">Precinct Margin: D+54%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-562">Voter 562's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 562 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;36'53"N 70&deg;51'18"W</div>
        <div class="text address"><i>approx.</i> 6, Widemarsh Beach Way, Wigwam Beach, Fairhaven, Bristol County, Massachusetts, 02719, United States</div>
        <div class="text dem precinct">Precinct Margin: D+2%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.835285718624625,-87.79899059349034&ll=41.835285718624625,-87.79899059349034&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/560.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.61491735,-70.85517898289856&ll=41.61491735,-70.85517898289856&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/561.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-563">Voter 563's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 563 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;17'9"N 81&deg;44'50"W</div>
        <div class="text address"><i>approx.</i> 6013, Manchester Place, Carlton Lakes, Collier County, Florida, 34110, United States</div>
        <div class="text gop precinct">Precinct Margin: R+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-564">Voter 564's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 564 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;38'46"N 116&deg;19'34"W</div>
        <div class="text address"><i>approx.</i> 4500, North Tumbleweed Lane, Boise, Ada County, Idaho, 83713, United States</div>
        <div class="text gop precinct">Precinct Margin: R+12%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.28587855638184,-81.7473881333283&ll=26.28587855638184,-81.7473881333283&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/562.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.6462855,-116.3262281&ll=43.6462855,-116.3262281&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/563.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-565">Voter 565's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 565 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;57'12"N 86&deg;42'47"W</div>
        <div class="text address"><i>approx.</i> 5087, Sky Blue Lane, Perry County, Indiana, 47520, United States</div>
        <div class="text gop precinct">Precinct Margin: R+30%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-566">Voter 566's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 566 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;50'43"N 72&deg;48'13"W</div>
        <div class="text address"><i>approx.</i> Patricia Lane, Manor Woods, Suffolk County, New York, 11949, United States</div>
        <div class="text gop precinct">Precinct Margin: R+31%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.95341022045564,-86.71321408799608&ll=37.95341022045564,-86.71321408799608&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/564.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.845434,-72.8037104&ll=40.845434,-72.8037104&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/565.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-567">Voter 567's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 567 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;46'44"N 73&deg;56'49"W</div>
        <div class="text address"><i>approx.</i> 402, East 90th Street, Manhattan Community Board 8, Yorkville, New York County, City of New York, New York, 10128, United States</div>
        <div class="text dem precinct">Precinct Margin: D+71%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-568">Voter 568's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 568 voted for Trump</div> -->
        <div class="text coordinate"> 26&deg;1'41"N 80&deg;16'26"W</div>
        <div class="text address"><i>approx.</i> 2151, Northwest 96th Terrace, Pembroke Pines, Broward County, Florida, 33024, United States</div>
        <div class="text dem precinct">Precinct Margin: D+25%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.77900135,-73.94697130106167&ll=40.77900135,-73.94697130106167&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/566.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.028229600000003,-80.2739028166902&ll=26.028229600000003,-80.2739028166902&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/567.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-569">Voter 569's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 569 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;47'57"N 74&deg;13'24"W</div>
        <div class="text address"><i>approx.</i> 47, Cedar Avenue, Montclair, Essex County, New Jersey, 07042, United States</div>
        <div class="text dem precinct">Precinct Margin: D+85%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-570">Voter 570's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 570 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;5'18"N 111&deg;56'26"W</div>
        <div class="text address"><i>approx.</i> East Primrose Lane, Hidden Hollow, Layton, Davis County, Utah, 84040, United States</div>
        <div class="text gop precinct">Precinct Margin: R+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.7992322,-74.2233832&ll=40.7992322,-74.2233832&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/568.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.0884933,-111.9405985&ll=41.0884933,-111.9405985&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/569.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-571">Voter 571's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 571 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;36'53"N 98&deg;28'39"W</div>
        <div class="text address"><i>approx.</i> Packsaddle Mountain Road, Llano County, Texas, 78639, United States</div>
        <div class="text gop precinct">Precinct Margin: R+51%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-572">Voter 572's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 572 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;52'2"N 77&deg;8'39"W</div>
        <div class="text address"><i>approx.</i> Arlington Boulevard Service Road, Seven Corners, Fairfax County, Virginia, 22044, United States</div>
        <div class="text dem precinct">Precinct Margin: D+48%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.61486257897811,-98.47763451684965&ll=30.61486257897811,-98.47763451684965&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/570.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.8673005463276,-77.1441892470575&ll=38.8673005463276,-77.1441892470575&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/571.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-573">Voter 573's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 573 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;32'37"N 80&deg;10'15"W</div>
        <div class="text address"><i>approx.</i> 7899, Rockford Road, Palm Beach County, Florida, 33472, United States</div>
        <div class="text dem precinct">Precinct Margin: D+33%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-574">Voter 574's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 574 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;48'26"N 81&deg;18'6"W</div>
        <div class="text address"><i>approx.</i> 365, Chigger Ridge Road, Alexander County, North Carolina, 28681, United States</div>
        <div class="text gop precinct">Precinct Margin: R+57%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.543839054513203,-80.17102266603655&ll=26.543839054513203,-80.17102266603655&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/572.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.8073899991316,-81.30182817743943&ll=35.8073899991316,-81.30182817743943&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/573.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-575">Voter 575's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 575 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;30'30"N 111&deg;55'29"W</div>
        <div class="text address"><i>approx.</i> FireSky Resort & Spa, East Chaparral Road, Scottsdale, Maricopa County, Arizona, 85251, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-576">Voter 576's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 576 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;44'30"N 93&deg;45'36"W</div>
        <div class="text address"><i>approx.</i> Cedar Farm Lane, Bossier Parish, Louisiana, 71064, United States</div>
        <div class="text gop precinct">Precinct Margin: R+63%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.508464399999994,-111.92493367617229&ll=33.508464399999994,-111.92493367617229&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/574.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.741903,-93.760124&ll=32.741903,-93.760124&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/575.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-577">Voter 577's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 577 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;56'25"N 76&deg;37'48"W</div>
        <div class="text address"><i>approx.</i> 641, Kendale Road, Freysville, Windsor Township, York County, Pennsylvania, 17356, United States</div>
        <div class="text gop precinct">Precinct Margin: R+29%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-578">Voter 578's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 578 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;39'18"N 93&deg;21'59"W</div>
        <div class="text address"><i>approx.</i> 303, Johnson Street, Albert Lea, Freeborn County, Minnesota, 56007, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.940360851288986,-76.6301590166924&ll=39.940360851288986,-76.6301590166924&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/576.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.6550186,-93.3665373&ll=43.6550186,-93.3665373&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/577.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-579">Voter 579's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 579 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;6'18"N 73&deg;33'18"W</div>
        <div class="text address"><i>approx.</i> 10, Hampshire Lane, Turn of River, Stamford, Fairfield County, Connecticut, 06905, United States</div>
        <div class="text dem precinct">Precinct Margin: D+21%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-580">Voter 580's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 580 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;6'47"N 77&deg;36'7"W</div>
        <div class="text address"><i>approx.</i> 17415, Chamberlands Road, Dinwiddie County, Virginia, 23841, United States</div>
        <div class="text gop precinct">Precinct Margin: R+33%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.1052254,-73.5551167&ll=41.1052254,-73.5551167&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/578.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.11327005,-77.60209174656967&ll=37.11327005,-77.60209174656967&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/579.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-581">Voter 581's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 581 voted for Biden</div> -->
        <div class="text coordinate"> 47&deg;53'45"N 122&deg;15'6"W</div>
        <div class="text address"><i>approx.</i> Arco, Evergreen Way, Fairmont, Holly, Everett, Snohomish County, Washington, 98204, United States</div>
        <div class="text dem precinct">Precinct Margin: D+33%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-582">Voter 582's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 582 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;19'33"N 80&deg;58'39"W</div>
        <div class="text address"><i>approx.</i> 1469, Hart Road, Shuffletown, Charlotte, Mecklenburg County, North Carolina, 28214, United States</div>
        <div class="text dem precinct">Precinct Margin: D+37%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.8960806,-122.2517647&ll=47.8960806,-122.2517647&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/580.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.3260491689719,-80.97769691786681&ll=35.3260491689719,-80.97769691786681&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/581.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-583">Voter 583's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 583 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;41'51"N 97&deg;39'17"W</div>
        <div class="text address"><i>approx.</i> 1819, Old Annetta Road, Annetta, Parker County, Texas, 76008, United States</div>
        <div class="text gop precinct">Precinct Margin: R+57%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-584">Voter 584's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 584 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;44'16"N 86&deg;5'54"W</div>
        <div class="text address"><i>approx.</i> 11829, Buttercup Circle, Granger, Saint Joseph County, Indiana, 46530, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.69770393193053,-97.65485325008038&ll=32.69770393193053,-97.65485325008038&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/582.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.73779360869565,-86.098586&ll=41.73779360869565,-86.098586&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/583.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-585">Voter 585's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 585 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;12'54"N 118&deg;26'10"W</div>
        <div class="text address"><i>approx.</i> 7944, Clearfield Avenue, Panorama City, Los Angeles, Los Angeles County, California, 91402, United States</div>
        <div class="text dem precinct">Precinct Margin: D+43%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-586">Voter 586's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 586 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;12'10"N 97&deg;9'32"W</div>
        <div class="text address"><i>approx.</i> Athletic Center (ATHC), South Bonnie Brae Street, Denton, Denton County, Texas, 76203, United States</div>
        <div class="text dem precinct">Precinct Margin: D+28%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.215257291731724,-118.43624045654815&ll=34.215257291731724,-118.43624045654815&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/584.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.202826599999995,-97.15901671395513&ll=33.202826599999995,-97.15901671395513&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/585.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-587">Voter 587's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 587 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;58'16"N 122&deg;17'42"W</div>
        <div class="text address"><i>approx.</i> 4501, Santa Rita Road, Richmond, Contra Costa County, California, 94803, United States</div>
        <div class="text dem precinct">Precinct Margin: D+69%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-588">Voter 588's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 588 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;38'58"N 93&deg;14'1"W</div>
        <div class="text address"><i>approx.</i> 20708, Hartford Way, Village Creek, Lakeville, Dakota County, Minnesota, 55044, United States</div>
        <div class="text gop precinct">Precinct Margin: R+4%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.971215,-122.295032&ll=37.971215,-122.295032&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/586.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.64954935,-93.23369412366517&ll=44.64954935,-93.23369412366517&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/587.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-589">Voter 589's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 589 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;18'4"N 98&deg;0'24"W</div>
        <div class="text address"><i>approx.</i> 955, Live Oak Street, Elsa, Hidalgo County, Texas, 78543, United States</div>
        <div class="text dem precinct">Precinct Margin: D+38%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-590">Voter 590's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 590 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;52'56"N 87&deg;14'5"W</div>
        <div class="text address"><i>approx.</i> Begeman Cemetery, East Wagner Road, Knox County, Indiana, 47596, United States</div>
        <div class="text gop precinct">Precinct Margin: R+60%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.30127032935797,-98.00677037164988&ll=26.30127032935797,-98.00677037164988&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/588.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.882268,-87.2347362&ll=38.882268,-87.2347362&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/589.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-591">Voter 591's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 591 voted for Biden</div> -->
        <div class="text coordinate"> 28&deg;36'5"N 81&deg;10'2"W</div>
        <div class="text address"><i>approx.</i> 4033, Shawn Circle, Tanner Crossings, Orange County, Florida, 32826, United States</div>
        <div class="text dem precinct">Precinct Margin: D+28%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-592">Voter 592's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 592 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;42'51"N 121&deg;56'53"W</div>
        <div class="text address"><i>approx.</i> 11554, Vista Place, West Dublin, Dublin, Alameda County, California, 94583, United States</div>
        <div class="text dem precinct">Precinct Margin: D+35%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=28.60146457101039,-81.1674413678588&ll=28.60146457101039,-81.1674413678588&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/590.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.714373949999995,-121.94814046782238&ll=37.714373949999995,-121.94814046782238&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/591.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-593">Voter 593's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 593 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;11'37"N 80&deg;42'50"W</div>
        <div class="text address"><i>approx.</i> 5116, Birchbark Lane, Silverstone, Charlotte, Mecklenburg County, North Carolina, 28227, United States</div>
        <div class="text dem precinct">Precinct Margin: D+43%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-594">Voter 594's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 594 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;10'15"N 117&deg;18'51"W</div>
        <div class="text address"><i>approx.</i> 1399, Jeffrey Way, San Bernardino, San Bernardino County, California, 92407, United States</div>
        <div class="text dem precinct">Precinct Margin: D+28%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.19362545835154,-80.71411277836614&ll=35.19362545835154,-80.71411277836614&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/592.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.170937,-117.314434&ll=34.170937,-117.314434&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/593.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-595">Voter 595's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 595 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;31'2"N 97&deg;44'33"W</div>
        <div class="text address"><i>approx.</i> Oak Brook Trail, Avery Ranch, Brushy Creek, Williamson County, Texas, 78781, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-596">Voter 596's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 596 voted for Trump</div> -->
        <div class="text coordinate"> 48&deg;12'4"N 96&deg;46'11"W</div>
        <div class="text address"><i>approx.</i> 683, North 4th Street, Warren, Marshall County, Minnesota, 56762, United States</div>
        <div class="text gop precinct">Precinct Margin: R+49%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.517363493202023,-97.7425172093583&ll=30.517363493202023,-97.7425172093583&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/594.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=48.20125987495309,-96.76976499035408&ll=48.20125987495309,-96.76976499035408&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/595.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-597">Voter 597's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 597 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;22'14"N 84&deg;23'49"W</div>
        <div class="text address"><i>approx.</i> Hamilton Mason Road, Liberty Township, Butler County, Ohio, 45069, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-598">Voter 598's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 598 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;7'47"N 115&deg;17'45"W</div>
        <div class="text address"><i>approx.</i> 9125, West Desert Inn Road, Las Vegas, Clark County, Nevada, 89117, United States</div>
        <div class="text dem precinct">Precinct Margin: D+15%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.37081874258132,-84.39718965520535&ll=39.37081874258132,-84.39718965520535&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/596.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.129902162050676,-115.29593224818038&ll=36.129902162050676,-115.29593224818038&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/597.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-599">Voter 599's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 599 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;42'37"N 73&deg;44'35"W</div>
        <div class="text address"><i>approx.</i> 106-19, 215th Street, Queens, City of New York, New York, 11429, United States</div>
        <div class="text dem precinct">Precinct Margin: D+88%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-600">Voter 600's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 600 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;10'42"N 75&deg;59'10"W</div>
        <div class="text address"><i>approx.</i> 545, Ewingtown Road, Queen Anne's County, Maryland, 21623, United States</div>
        <div class="text gop precinct">Precinct Margin: R+26%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.71053975,-73.74322893529411&ll=40.71053975,-73.74322893529411&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/598.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.17854664096569,-75.9863751394704&ll=39.17854664096569,-75.9863751394704&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/599.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-601">Voter 601's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 601 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;48'58"N 117&deg;55'51"W</div>
        <div class="text address"><i>approx.</i> Goodhue Avenue, Anaheim, Orange County, California, 92802, United States</div>
        <div class="text dem precinct">Precinct Margin: D+29%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-602">Voter 602's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 602 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;0'47"N 95&deg;54'53"W</div>
        <div class="text address"><i>approx.</i> 10321, South Granite Avenue, Forest Pointe, Tulsa, Tulsa County, Oklahoma, 74137, United States</div>
        <div class="text gop precinct">Precinct Margin: R+44%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.81626883168665,-117.93092877097122&ll=33.81626883168665,-117.93092877097122&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/600.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.01328,-95.914764&ll=36.01328,-95.914764&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/601.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-603">Voter 603's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 603 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;1'39"N 118&deg;6'51"W</div>
        <div class="text address"><i>approx.</i> 865, Marconi Street, Montebello, Los Angeles County, California, 90640, United States</div>
        <div class="text dem precinct">Precinct Margin: D+34%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-604">Voter 604's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 604 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;11'14"N 121&deg;22'19"W</div>
        <div class="text address"><i>approx.</i> Mariposa lane, Deschutes County, Oregon, United States</div>
        <div class="text gop precinct">Precinct Margin: R+18%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.02763202936848,-118.11420919327327&ll=34.02763202936848,-118.11420919327327&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/602.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.18732435,-121.3720904941359&ll=44.18732435,-121.3720904941359&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/603.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-605">Voter 605's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 605 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;4'48"N 94&deg;11'47"W</div>
        <div class="text address"><i>approx.</i> 1239, North West End Avenue, Fayetteville, Washington County, Arkansas, 72703, United States</div>
        <div class="text dem precinct">Precinct Margin: D+44%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-606">Voter 606's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 606 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;57'45"N 93&deg;44'26"W</div>
        <div class="text address"><i>approx.</i> 1036, Autumn Lane, Aurora, Lawrence County, Missouri, 65605, United States</div>
        <div class="text gop precinct">Precinct Margin: R+60%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.08014933333333,-94.196624&ll=36.08014933333333,-94.196624&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/604.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.962681,-93.740753&ll=36.962681,-93.740753&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/605.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-607">Voter 607's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 607 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;59'5"N 97&deg;22'30"W</div>
        <div class="text address"><i>approx.</i> 809, Mexicali Way, Fort Worth, Tarrant County, Texas, 76052, United States</div>
        <div class="text gop precinct">Precinct Margin: R+26%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-608">Voter 608's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 608 voted for Trump</div> -->
        <div class="text coordinate"> 25&deg;51'14"N 80&deg;8'18"W</div>
        <div class="text address"><i>approx.</i> 7300, Trouville Esplanade, Isle of Normandy, Miami Beach, Miami-Dade County, Florida, 33141, United States</div>
        <div class="text dem precinct">Precinct Margin: D+34%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.98490015,-97.37509659516424&ll=32.98490015,-97.37509659516424&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/606.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=25.85412815,-80.1383497899731&ll=25.85412815,-80.1383497899731&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/607.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-609">Voter 609's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 609 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;2'28"N 95&deg;52'22"W</div>
        <div class="text address"><i>approx.</i> Meadowbrook Country Club, 9300, East 81st Street, Highland Park, Tulsa, Tulsa County, Oklahoma, 74014, United States</div>
        <div class="text gop precinct">Precinct Margin: R+21%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-610">Voter 610's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 610 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;35'12"N 98&deg;29'31"W</div>
        <div class="text address"><i>approx.</i> 241, Limestone Creek, Hill Country Village, Bexar County, Texas, 78232, United States</div>
        <div class="text gop precinct">Precinct Margin: R+34%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.0412137,-95.87302346249669&ll=36.0412137,-95.87302346249669&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/608.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.58682398795251,-98.49212856889723&ll=29.58682398795251,-98.49212856889723&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/609.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-611">Voter 611's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 611 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;15'6"N 83&deg;26'29"W</div>
        <div class="text address"><i>approx.</i> I-275 Metro Trail, Van Buren Township, Wayne County, Michigan, 48111-1133, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-612">Voter 612's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 612 voted for Trump</div> -->
        <div class="text coordinate"> 25&deg;38'3"N 80&deg;19'56"W</div>
        <div class="text address"><i>approx.</i> 8520, Southwest 146th Street, Rockdale, Palmetto Bay, Miami-Dade County, Florida, 33158, United States</div>
        <div class="text dem precinct">Precinct Margin: D+13%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.25189372170181,-83.44154687481392&ll=42.25189372170181,-83.44154687481392&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/610.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=25.6344166,-80.3324202&ll=25.6344166,-80.3324202&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/611.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-613">Voter 613's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 613 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;41'14"N 105&deg;4'26"W</div>
        <div class="text address"><i>approx.</i> 6945, West Calahan Avenue, Lakewood, Jefferson County, Colorado, 80232, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-614">Voter 614's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 614 voted for Trump</div> -->
        <div class="text coordinate"> 28&deg;8'38"N 82&deg;30'52"W</div>
        <div class="text address"><i>approx.</i> 4126, Highland Park Circle, Cheval, Hillsborough County, Florida, 33558, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.68743208461128,-105.07388901721828&ll=39.68743208461128,-105.07388901721828&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/612.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=28.1440171828119,-82.51460445737219&ll=28.1440171828119,-82.51460445737219&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/613.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-615">Voter 615's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 615 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;46'52"N 89&deg;54'21"W</div>
        <div class="text address"><i>approx.</i> 7715, Goshen Road, Edwardsville, Madison County, Illinois, 62025, United States</div>
        <div class="text gop precinct">Precinct Margin: R+29%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-616">Voter 616's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 616 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;18'27"N 97&deg;18'58"W</div>
        <div class="text address"><i>approx.</i> 16198, A Street, Cleveland County, Oklahoma, 73165, United States</div>
        <div class="text gop precinct">Precinct Margin: R+50%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.781265576141394,-89.90605171422997&ll=38.781265576141394,-89.90605171422997&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/614.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.307621,-97.316169&ll=35.307621,-97.316169&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/615.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-617">Voter 617's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 617 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;37'34"N 97&deg;23'38"W</div>
        <div class="text address"><i>approx.</i> 4616, Bellflower Way, Fort Worth, Tarrant County, Texas, 76123, United States</div>
        <div class="text dem precinct">Precinct Margin: D+16%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-618">Voter 618's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 618 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;59'59"N 121&deg;30'24"W</div>
        <div class="text address"><i>approx.</i> 2911, Leach Road, Yuba County, California, 95961, United States</div>
        <div class="text gop precinct">Precinct Margin: R+56%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.62616815,-97.39398365635361&ll=32.62616815,-97.39398365635361&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/616.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.999894,-121.506779&ll=38.999894,-121.506779&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/617.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-619">Voter 619's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 619 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;38'15"N 76&deg;53'37"W</div>
        <div class="text address"><i>approx.</i> Salvation Army, Old Washington Road, Mattawoman, Waldorf, Charles County, Maryland, 20601, United States</div>
        <div class="text dem precinct">Precinct Margin: D+59%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-620">Voter 620's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 620 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;34'7"N 74&deg;23'18"W</div>
        <div class="text address"><i>approx.</i> 30, Westwood Circle, New Petrograd, Edison, Middlesex County, New Jersey, 08820, United States</div>
        <div class="text dem precinct">Precinct Margin: D+24%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.63764765,-76.8937801980391&ll=38.63764765,-76.8937801980391&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/618.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.56881777585239,-74.3884852355324&ll=40.56881777585239,-74.3884852355324&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/619.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-621">Voter 621's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 621 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;40'21"N 121&deg;47'52"W</div>
        <div class="text address"><i>approx.</i> 341, Jillana Avenue, Livermore, Alameda County, California, 94550, United States</div>
        <div class="text dem precinct">Precinct Margin: D+27%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-622">Voter 622's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 622 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;37'28"N 72&deg;54'57"W</div>
        <div class="text address"><i>approx.</i> E Grannis Pond Road, Southington, Hartford County, Connecticut, 06010-7403, United States</div>
        <div class="text gop precinct">Precinct Margin: R+13%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.67268763861467,-121.7978709790018&ll=37.67268763861467,-121.7978709790018&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/620.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.624584,-72.916017&ll=41.624584,-72.916017&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/621.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-623">Voter 623's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 623 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;5'20"N 81&deg;35'59"W</div>
        <div class="text address"><i>approx.</i> 499, Chattan Way, Fruit Cove, Saint Johns County, Florida, 32259, United States</div>
        <div class="text gop precinct">Precinct Margin: R+34%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-624">Voter 624's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 624 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;25'52"N 118&deg;45'29"W</div>
        <div class="text address"><i>approx.</i> 1297, Lazy Heart Lane, Churchill County, Nevada, 89406, United States</div>
        <div class="text gop precinct">Precinct Margin: R+68%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.088988467931774,-81.5998010127644&ll=30.088988467931774,-81.5998010127644&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/622.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.43114185490474,-118.75831954691041&ll=39.43114185490474,-118.75831954691041&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/623.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-625">Voter 625's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 625 voted for Biden</div> -->
        <div class="text coordinate"> 31&deg;27'40"N 110&deg;17'39"W</div>
        <div class="text address"><i>approx.</i> 489, East Ramsey Canyon Road, Bledsoe, Sierra Vista, Cochise County, Arizona, 85615, United States</div>
        <div class="text gop precinct">Precinct Margin: R+35%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-626">Voter 626's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 626 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;33'32"N 94&deg;40'58"W</div>
        <div class="text address"><i>approx.</i> 685, Hickory Creek Lane, Harrison County, Texas, 75605, United States</div>
        <div class="text gop precinct">Precinct Margin: R+75%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=31.4612177,-110.2941749&ll=31.4612177,-110.2941749&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/624.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.5588941,-94.6828708&ll=32.5588941,-94.6828708&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/625.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-627">Voter 627's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 627 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;31'50"N 74&deg;16'24"W</div>
        <div class="text address"><i>approx.</i> 913, Amboy Avenue, William Dunlap Homes, Perth Amboy, Woodbridge, Middlesex County, New Jersey, 08861, United States</div>
        <div class="text dem precinct">Precinct Margin: D+44%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-628">Voter 628's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 628 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;35'50"N 98&deg;26'15"W</div>
        <div class="text address"><i>approx.</i> 17115, Redland Road, San Antonio, Bexar County, Texas, 78247, United States</div>
        <div class="text gop precinct">Precinct Margin: R+4%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.530636,-74.273459&ll=40.530636,-74.273459&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/626.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.597418,-98.437755&ll=29.597418,-98.437755&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/627.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-629">Voter 629's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 629 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;41'50"N 96&deg;49'58"W</div>
        <div class="text address"><i>approx.</i> 542, Early Dawn Trail, Dallas, Dallas County, Texas, 75224, United States</div>
        <div class="text dem precinct">Precinct Margin: D+49%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-630">Voter 630's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 630 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;18'38"N 84&deg;14'55"W</div>
        <div class="text address"><i>approx.</i> 9458, Old Woodville Road, Lutterloh, Woodville, Leon County, Florida, 32305, United States</div>
        <div class="text gop precinct">Precinct Margin: R+5%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.697384299999996,-96.83294099786863&ll=32.697384299999996,-96.83294099786863&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/628.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.310807454782093,-84.24888615697454&ll=30.310807454782093,-84.24888615697454&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/629.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-631">Voter 631's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 631 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;55'33"N 88&deg;2'24"W</div>
        <div class="text address"><i>approx.</i> 6551, Parkedge Circle, Franklin, Milwaukee County, Wisconsin, 53132, United States</div>
        <div class="text gop precinct">Precinct Margin: R+1%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-632">Voter 632's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 632 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;24'2"N 88&deg;10'29"W</div>
        <div class="text address"><i>approx.</i> 69, Cora Road, Fox Lake, Lake County, Illinois, 60020, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.92606282258843,-88.04017055849641&ll=42.92606282258843,-88.04017055849641&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/630.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.400778,-88.17488&ll=42.400778,-88.17488&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/631.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-633">Voter 633's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 633 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;39'51"N 94&deg;16'0"W</div>
        <div class="text address"><i>approx.</i> Buckhorn Road, Harrison County, Texas, United States</div>
        <div class="text gop precinct">Precinct Margin: R+60%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-634">Voter 634's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 634 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;37'21"N 123&deg;4'28"W</div>
        <div class="text address"><i>approx.</i> 2502, 20th Avenue Southeast, Albany, Linn County, Oregon, 97322, United States</div>
        <div class="text dem precinct">Precinct Margin: D+2%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.664414,-94.2667783&ll=32.664414,-94.2667783&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/632.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.62264407325634,-123.07466516075581&ll=44.62264407325634,-123.07466516075581&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/633.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-635">Voter 635's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 635 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;45'5"N 87&deg;37'33"W</div>
        <div class="text address"><i>approx.</i> South Lafayette Avenue, Greater Grand Crossing, Chicago, Lake Township, Cook County, Illinois, 60621, United States</div>
        <div class="text dem precinct">Precinct Margin: D+96%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-636">Voter 636's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 636 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;9'3"N 84&deg;4'48"W</div>
        <div class="text address"><i>approx.</i> 5698, Freedom Trail, Jackson Township, Clermont County, Ohio, 45103, United States</div>
        <div class="text gop precinct">Precinct Margin: R+69%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.75141634704331,-87.62585696081815&ll=41.75141634704331,-87.62585696081815&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/634.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.151109,-84.080195&ll=39.151109,-84.080195&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/635.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-637">Voter 637's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 637 voted for Biden</div> -->
        <div class="text coordinate"> 46&deg;57'27"N 123&deg;46'26"W</div>
        <div class="text address"><i>approx.</i> 170, H Street, Cosmopolis, Grays Harbor County, Washington, 98537, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-638">Voter 638's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 638 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;25'6"N 79&deg;6'5"W</div>
        <div class="text address"><i>approx.</i> John Brewer Road, Person County, North Carolina, 27291, United States</div>
        <div class="text gop precinct">Precinct Margin: R+47%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=46.957585,-123.77408985714285&ll=46.957585,-123.77408985714285&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/636.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.418502635257724,-79.10154947920834&ll=36.418502635257724,-79.10154947920834&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/637.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-639">Voter 639's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 639 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;9'34"N 76&deg;39'40"W</div>
        <div class="text address"><i>approx.</i> BWI Trail, Glenbrook, Glen Burnie, Anne Arundel County, Maryland, 21144-1320, United States</div>
        <div class="text dem precinct">Precinct Margin: D+13%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-640">Voter 640's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 640 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;6'41"N 117&deg;11'4"W</div>
        <div class="text address"><i>approx.</i> Beattie Middle School, Boulder Avenue, Highland, San Bernardino County, California, 92346-9998, United States</div>
        <div class="text dem precinct">Precinct Margin: D+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.159654201978576,-76.66122878474124&ll=39.159654201978576,-76.66122878474124&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/638.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.111648200000005,-117.1846914026153&ll=34.111648200000005,-117.1846914026153&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/639.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-641">Voter 641's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 641 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;41'17"N 81&deg;16'7"W</div>
        <div class="text address"><i>approx.</i> 10325, Johnnycake Ridge Road, Lake County, Ohio, 44077, United States</div>
        <div class="text gop precinct">Precinct Margin: R+25%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-642">Voter 642's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 642 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;0'18"N 114&deg;56'16"W</div>
        <div class="text address"><i>approx.</i> 1268, Dalene Avenue, River Mountain View Estates, Henderson, Clark County, Nevada, 89002, United States</div>
        <div class="text gop precinct">Precinct Margin: R+28%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.688165679036274,-81.26880781874272&ll=41.688165679036274,-81.26880781874272&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/640.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.00521674011829,-114.93804806248185&ll=36.00521674011829,-114.93804806248185&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/641.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-643">Voter 643's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 643 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;33'48"N 105&deg;6'9"W</div>
        <div class="text address"><i>approx.</i> Falcon Bluffs Middle School, West Remington Place, Jefferson County, Colorado, 80127-5008, United States</div>
        <div class="text dem precinct">Precinct Margin: D+5%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-644">Voter 644's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 644 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;53'4"N 75&deg;31'8"W</div>
        <div class="text address"><i>approx.</i> 809, Concord Road, Concordville, Concord Township, Delaware County, Pennsylvania, 19342, United States</div>
        <div class="text dem precinct">Precinct Margin: D+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.563388,-105.10261011954066&ll=39.563388,-105.10261011954066&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/642.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.88445402175487,-75.51897034056213&ll=39.88445402175487,-75.51897034056213&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/643.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-645">Voter 645's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 645 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;8'26"N 81&deg;46'32"W</div>
        <div class="text address"><i>approx.</i> 1378, Embassy Lane, Collier County, Florida, 34104, United States</div>
        <div class="text gop precinct">Precinct Margin: R+21%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-646">Voter 646's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 646 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;42'21"N 118&deg;0'59"W</div>
        <div class="text address"><i>approx.</i> 17641, Collins Circle, Huntington Beach, Orange County, California, 92647, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.14080075030264,-81.77568548939743&ll=26.14080075030264,-81.77568548939743&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/644.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.706013,-118.01642630048056&ll=33.706013,-118.01642630048056&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/645.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-647">Voter 647's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 647 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;33'50"N 117&deg;3'0"W</div>
        <div class="text address"><i>approx.</i> Smythe Elementary School, 1880, Smythe Avenue, San Ysidro, San Diego, San Diego County, California, 92173, United States</div>
        <div class="text dem precinct">Precinct Margin: D+51%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-648">Voter 648's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 648 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;28'5"N 86&deg;32'2"W</div>
        <div class="text address"><i>approx.</i> 551, Golson Road, Prattville, Autauga County, Alabama, 36067, United States</div>
        <div class="text gop precinct">Precinct Margin: R+38%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.563896,-117.05020915178292&ll=32.563896,-117.05020915178292&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/646.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.468332865421104,-86.5340470615719&ll=32.468332865421104,-86.5340470615719&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/647.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-649">Voter 649's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 649 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;28'0"N 96&deg;47'9"W</div>
        <div class="text address"><i>approx.</i> Birchwood Manor, 1120, Walnut Street, North Bend, Dodge County, Nebraska, 68649, United States</div>
        <div class="text gop precinct">Precinct Margin: R+45%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-650">Voter 650's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 650 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;6'46"N 79&deg;59'36"W</div>
        <div class="text address"><i>approx.</i> 63, Bear Run Road, Philippi, Barbour County, West Virginia, 26416, United States</div>
        <div class="text gop precinct">Precinct Margin: R+57%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.4667031,-96.7859214&ll=41.4667031,-96.7859214&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/648.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.11286866384781,-79.99333355039035&ll=39.11286866384781,-79.99333355039035&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/649.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-651">Voter 651's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 651 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;52'5"N 88&deg;6'29"W</div>
        <div class="text address"><i>approx.</i> 201, West Union Avenue, Wheaton, DuPage County, Illinois, 60187, United States</div>
        <div class="text dem precinct">Precinct Margin: D+27%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-652">Voter 652's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 652 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;0'41"N 115&deg;13'4"W</div>
        <div class="text address"><i>approx.</i> Aldeane Comito Ries Elementary School, West Le Baron Avenue, Enterprise, Clark County, Nevada, 89141, United States</div>
        <div class="text dem precinct">Precinct Margin: D+10%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.868116,-88.108075&ll=41.868116,-88.108075&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/650.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.011488850000006,-115.21781129372306&ll=36.011488850000006,-115.21781129372306&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/651.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-653">Voter 653's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 653 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;10'21"N 81&deg;4'9"W</div>
        <div class="text address"><i>approx.</i> Volusia County Parks, Recreation & Cultural Maintenance / Tradeworkers, Bellevue Avenue, Daytona Beach, Volusia County, Florida, 32124, United States</div>
        <div class="text gop precinct">Precinct Margin: R+20%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-654">Voter 654's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 654 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;9'19"N 72&deg;28'20"W</div>
        <div class="text address"><i>approx.</i> Healthsouth Rehabilitation Hospital Of Western Massachusetts, 222, State Street, Ludlow, Hampden County, Massachusetts, 01056, United States</div>
        <div class="text dem precinct">Precinct Margin: D+2%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.1725303,-81.06940062893909&ll=29.1725303,-81.06940062893909&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/652.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.15547415,-72.47222226666076&ll=42.15547415,-72.47222226666076&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/653.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-655">Voter 655's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 655 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;51'59"N 119&deg;46'11"W</div>
        <div class="text address"><i>approx.</i> 722, East Shepherd Avenue, Fresno, Fresno County, California, 93720, United States</div>
        <div class="text dem precinct">Precinct Margin: D+1%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-656">Voter 656's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 656 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;34'3"N 68&deg;13'12"W</div>
        <div class="text address"><i>approx.</i> 1032, Kingman Road, Kingman Township, Penobscot County, Maine, 04451, United States</div>
        <div class="text gop precinct">Precinct Margin: R+76%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.866449363319745,-119.76987479799588&ll=36.866449363319745,-119.76987479799588&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/654.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.5676455,-68.22013307371795&ll=45.5676455,-68.22013307371795&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/655.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-657">Voter 657's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 657 voted for Biden</div> -->
        <div class="text coordinate"> 47&deg;33'33"N 122&deg;22'10"W</div>
        <div class="text address"><i>approx.</i> West Seattle Golf Course, Southwest Avalon Way, West Seattle, Seattle, King County, Washington, 98126, United States</div>
        <div class="text dem precinct">Precinct Margin: D+81%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-658">Voter 658's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 658 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;25'32"N 69&deg;26'16"W</div>
        <div class="text address"><i>approx.</i> 1180, North Palermo Road, Carrs Corner, Palermo, Waldo County, Maine, 04354, United States</div>
        <div class="text gop precinct">Precinct Margin: R+16%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.55940095,-122.36968270997508&ll=47.55940095,-122.36968270997508&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/656.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.4256832,-69.4378214&ll=44.4256832,-69.4378214&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/657.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-659">Voter 659's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 659 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;2'22"N 81&deg;25'39"W</div>
        <div class="text address"><i>approx.</i> Ripley Avenue, Springfield Township, Summit County, Ohio, 44312, United States</div>
        <div class="text gop precinct">Precinct Margin: R+30%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-660">Voter 660's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 660 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;38'34"N 111&deg;50'49"W</div>
        <div class="text address"><i>approx.</i> 6099, East Village Road, Murray, Salt Lake County, Utah, 84121, United States</div>
        <div class="text dem precinct">Precinct Margin: D+16%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.0394969,-81.4276737&ll=41.0394969,-81.4276737&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/658.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.642893,-111.846976&ll=40.642893,-111.846976&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/659.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-661">Voter 661's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 661 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;49'56"N 74&deg;9'40"W</div>
        <div class="text address"><i>approx.</i> 60, Oakley Terrace, Nutley, Essex County, New Jersey, 07110, United States</div>
        <div class="text dem precinct">Precinct Margin: D+3%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-662">Voter 662's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 662 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;49'31"N 71&deg;18'43"W</div>
        <div class="text address"><i>approx.</i> 49, Londonderry Road, Windham, Rockingham County, New Hampshire, 03087, United States</div>
        <div class="text gop precinct">Precinct Margin: R+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.83245380645161,-74.16118870967742&ll=40.83245380645161,-74.16118870967742&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/660.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.825363499999995,-71.31195220562338&ll=42.825363499999995,-71.31195220562338&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/661.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-663">Voter 663's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 663 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;13'25"N 71&deg;19'20"W</div>
        <div class="text address"><i>approx.</i> Indian Stream Road, Pittsburg, Coos County, New Hampshire, United States</div>
        <div class="text gop precinct">Precinct Margin: R+41%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-664">Voter 664's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 664 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;15'20"N 84&deg;25'36"W</div>
        <div class="text address"><i>approx.</i> 707, Eaton Street, Jackson, Jackson County, Michigan, 49202, United States</div>
        <div class="text dem precinct">Precinct Margin: D+7%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.22379332622627,-71.32225073877783&ll=45.22379332622627,-71.32225073877783&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/662.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.255754383838386,-84.42694064646464&ll=42.255754383838386,-84.42694064646464&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/663.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-665">Voter 665's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 665 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;5'40"N 80&deg;9'58"W</div>
        <div class="text address"><i>approx.</i> 487, Quail Haven Lane, Woodbridge, Winston-Salem, Forsyth County, North Carolina, 27107, United States</div>
        <div class="text dem precinct">Precinct Margin: D+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-666">Voter 666's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 666 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;35'33"N 71&deg;7'11"W</div>
        <div class="text address"><i>approx.</i> 5, Voke Street, North Reading, Middlesex County, Massachusetts, 01864-1298, United States</div>
        <div class="text dem precinct">Precinct Margin: D+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.09462002601105,-80.16620313424853&ll=36.09462002601105,-80.16620313424853&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/664.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.59275155,-71.11986264909652&ll=42.59275155,-71.11986264909652&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/665.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-667">Voter 667's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 667 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;53'36"N 75&deg;19'44"W</div>
        <div class="text address"><i>approx.</i> 512, 6th Avenue, Folsom, Ridley Township, Delaware County, Pennsylvania, 19033, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-668">Voter 668's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 668 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;27'0"N 94&deg;25'1"W</div>
        <div class="text address"><i>approx.</i> 3, Park Place, Boston, New Boston, Bowie County, Texas, 75570, United States</div>
        <div class="text gop precinct">Precinct Margin: R+65%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.89357892850853,-75.32908448260869&ll=39.89357892850853,-75.32908448260869&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/666.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.4501735,-94.4169775&ll=33.4501735,-94.4169775&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/667.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-669">Voter 669's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 669 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;52'16"N 84&deg;32'4"W</div>
        <div class="text address"><i>approx.</i> Smyrna Fire Station #2, 642, Concord Road Southeast, Smyrna, Cobb County, Georgia, 30080, United States</div>
        <div class="text dem precinct">Precinct Margin: D+21%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-670">Voter 670's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 670 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;18'50"N 93&deg;35'25"W</div>
        <div class="text address"><i>approx.</i> 12619, Meadowvale Road Northwest, Elk River, Sherburne County, Minnesota, 55330, United States</div>
        <div class="text gop precinct">Precinct Margin: R+16%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.8713145,-84.5344693&ll=33.8713145,-84.5344693&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/668.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.31412365306122,-93.59053589795919&ll=45.31412365306122,-93.59053589795919&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/669.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-671">Voter 671's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 671 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;17'4"N 93&deg;25'20"W</div>
        <div class="text address"><i>approx.</i> 17319, Sodium Street Northwest, Ramsey, Anoka County, Minnesota, 55303, United States</div>
        <div class="text gop precinct">Precinct Margin: R+16%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-672">Voter 672's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 672 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;2'39"N 106&deg;29'46"W</div>
        <div class="text address"><i>approx.</i> 1108, Castellano Road Southeast, Four Hills (HOA), Albuquerque, Bernalillo County, New Mexico, 87123, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.284713,-93.422403&ll=45.284713,-93.422403&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/670.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.04442641209038,-106.49616328612363&ll=35.04442641209038,-106.49616328612363&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/671.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-673">Voter 673's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 673 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;40'42"N 112&deg;0'6"W</div>
        <div class="text address"><i>approx.</i> 4698, Condie Park Circle, West Valley City, Salt Lake County, Utah, 84120, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-674">Voter 674's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 674 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;5'23"N 82&deg;50'3"W</div>
        <div class="text address"><i>approx.</i> 5298, Hanover Close, Columbus, Sharon, Franklin County, Ohio, 43054, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.678375,-112.001676&ll=40.678375,-112.001676&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/672.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.089873,-82.834279&ll=40.089873,-82.834279&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/673.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-675">Voter 675's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 675 voted for Biden</div> -->
        <div class="text coordinate"> 43&deg;4'11"N 87&deg;52'32"W</div>
        <div class="text address"><i>approx.</i> 2838, North Summit Avenue, Historic Water Tower Neighborhood, Milwaukee, Milwaukee County, Wisconsin, 53211, United States</div>
        <div class="text dem precinct">Precinct Margin: D+55%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-676">Voter 676's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 676 voted for Trump</div> -->
        <div class="text coordinate"> 48&deg;12'0"N 106&deg;31'59"W</div>
        <div class="text address"><i>approx.</i> Pliley Road, Valley County, Montana, United States</div>
        <div class="text gop precinct">Precinct Margin: R+62%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.06997174684911,-87.87558019773303&ll=43.06997174684911,-87.87558019773303&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/674.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=48.200038888813424,-106.53321666802601&ll=48.200038888813424,-106.53321666802601&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/675.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-677">Voter 677's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 677 voted for Biden</div> -->
        <div class="text coordinate"> 27&deg;47'41"N 82&deg;19'41"W</div>
        <div class="text address"><i>approx.</i> 10869, Candle Stick Lane, Hillsborough County, Florida, 33579, United States</div>
        <div class="text dem precinct">Precinct Margin: D+18%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-678">Voter 678's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 678 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;3'16"N 118&deg;10'27"W</div>
        <div class="text address"><i>approx.</i> 1393, Machado Avenue, East Los Angeles, Los Angeles County, California, 90063, United States</div>
        <div class="text dem precinct">Precinct Margin: D+68%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=27.794981346938776,-82.3282531632653&ll=27.794981346938776,-82.3282531632653&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/676.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.0545532499079,-118.1741773245445&ll=34.0545532499079,-118.1741773245445&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/677.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-679">Voter 679's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 679 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;52'24"N 87&deg;42'31"W</div>
        <div class="text address"><i>approx.</i> 3303-3305, West Harrison Street, East Garfield Park, Chicago, Cook County, Illinois, 60624, United States</div>
        <div class="text dem precinct">Precinct Margin: D+94%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-680">Voter 680's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 680 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;1'58"N 120&deg;7'27"W</div>
        <div class="text address"><i>approx.</i> Avenue 19 1/2, Madera County, California, 93638-0299, United States</div>
        <div class="text dem precinct">Precinct Margin: D+1%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.8733912,-87.70887505245372&ll=41.8733912,-87.70887505245372&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/678.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.0328072,-120.1241698&ll=37.0328072,-120.1241698&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/679.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-681">Voter 681's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 681 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;38'56"N 90&deg;15'36"W</div>
        <div class="text address"><i>approx.</i> 4750, Westminster Place, Central West End, Saint Louis, Missouri, 63108, United States</div>
        <div class="text dem precinct">Precinct Margin: D+75%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-682">Voter 682's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 682 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;28'37"N 88&deg;54'8"W</div>
        <div class="text address"><i>approx.</i> 1370, South Towanda Barnes Road, McLean County, Illinois, 61704, United States</div>
        <div class="text gop precinct">Precinct Margin: R+31%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.64907485,-90.26026695341285&ll=38.64907485,-90.26026695341285&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/680.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.47717886872915,-88.90234528351544&ll=40.47717886872915,-88.90234528351544&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/681.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-683">Voter 683's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 683 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;36'44"N 75&deg;10'34"W</div>
        <div class="text address"><i>approx.</i> Remington Recreation Trail, Canton, Town of Canton, Saint Lawrence County, New York, 13617, United States</div>
        <div class="text dem precinct">Precinct Margin: D+42%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-684">Voter 684's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 684 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;48'9"N 84&deg;7'55"W</div>
        <div class="text address"><i>approx.</i> Benton MacKaye Trail, Union County, Georgia, United States</div>
        <div class="text gop precinct">Precinct Margin: R+66%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.612245713052346,-75.17635027653377&ll=44.612245713052346,-75.17635027653377&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/682.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.8025502,-84.1319931&ll=34.8025502,-84.1319931&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/683.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-685">Voter 685's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 685 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;13'33"N 80&deg;17'38"W</div>
        <div class="text address"><i>approx.</i> 399, Traub Road, Lee County, South Carolina, 29010, United States</div>
        <div class="text dem precinct">Precinct Margin: D+10%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-686">Voter 686's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 686 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;15'48"N 90&deg;7'24"W</div>
        <div class="text address"><i>approx.</i> 2388, Milam Street, Randall Place, Cunningham Heights, Pearl, Rankin County, Mississippi, 39208, United States</div>
        <div class="text gop precinct">Precinct Margin: R+37%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.2258412,-80.2940492&ll=34.2258412,-80.2940492&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/684.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.26351029144324,-90.12349039307229&ll=32.26351029144324,-90.12349039307229&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/685.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-687">Voter 687's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 687 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;20'46"N 80&deg;39'7"W</div>
        <div class="text address"><i>approx.</i> 699, Pitts School Road Southwest, Concord, Cabarrus County, North Carolina, 28027, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-688">Voter 688's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 688 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;13'6"N 72&deg;16'56"W</div>
        <div class="text address"><i>approx.</i> Palmer Wildlife Management Area, Rondeau Street, Palmer, Hampden County, Massachusetts, 01092, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.346148,-80.652097&ll=35.346148,-80.652097&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/686.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.2185219,-72.28249209624417&ll=42.2185219,-72.28249209624417&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/687.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-689">Voter 689's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 689 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;20'42"N 72&deg;56'30"W</div>
        <div class="text address"><i>approx.</i> 63, Sunset Road, Pine Rock, Hamden, New Haven County, Connecticut, 06514, United States</div>
        <div class="text dem precinct">Precinct Margin: D+74%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-690">Voter 690's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 690 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;32'48"N 82&deg;8'2"W</div>
        <div class="text address"><i>approx.</i> Columbia County Amphitheater, Evans Library Walking Path, Evans, Columbia County, Georgia, United States</div>
        <div class="text gop precinct">Precinct Margin: R+28%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.3450521,-72.9418057&ll=41.3450521,-72.9418057&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/688.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.5469171,-82.1341159&ll=33.5469171,-82.1341159&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/689.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-691">Voter 691's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 691 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;54'25"N 82&deg;57'45"W</div>
        <div class="text address"><i>approx.</i> 1191, Evergreen Road, Columbus, Franklin County, Ohio, 43207, United States</div>
        <div class="text dem precinct">Precinct Margin: D+77%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-692">Voter 692's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 692 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;10'9"N 88&deg;3'14"W</div>
        <div class="text address"><i>approx.</i> 21899, West Hidden Valley Road, Hidden Valley, Kildeer, Lake County, Illinois, 60047, United States</div>
        <div class="text gop precinct">Precinct Margin: R+1%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.90718541875917,-82.96277332275399&ll=39.90718541875917,-82.96277332275399&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/690.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.169378,-88.054005&ll=42.169378,-88.054005&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/691.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-693">Voter 693's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 693 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;51'52"N 80&deg;59'32"W</div>
        <div class="text address"><i>approx.</i> 188, Liberty Hill Road, Statesville, Iredell County, North Carolina, 28625, United States</div>
        <div class="text gop precinct">Precinct Margin: R+54%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-694">Voter 694's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 694 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;53'40"N 88&deg;1'52"W</div>
        <div class="text address"><i>approx.</i> 516, West Meadow Avenue, Lombard, DuPage County, Illinois, 60148, United States</div>
        <div class="text dem precinct">Precinct Margin: D+16%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.8646345,-80.9924319&ll=35.8646345,-80.9924319&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/692.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.89462046153846,-88.03127253846154&ll=41.89462046153846,-88.03127253846154&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/693.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-695">Voter 695's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 695 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;17'24"N 97&deg;44'41"W</div>
        <div class="text address"><i>approx.</i> 2508, Rio Grande Street, The Drag, Austin, Travis County, Texas, 78705-5609, United States</div>
        <div class="text dem precinct">Precinct Margin: D+67%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-696">Voter 696's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 696 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;26'33"N 88&deg;7'13"W</div>
        <div class="text address"><i>approx.</i> 1918, Sandy Springs Road, Town of Lawrence, Brown County, Wisconsin, 54115, United States</div>
        <div class="text gop precinct">Precinct Margin: R+23%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.290211,-97.74493049658622&ll=30.290211,-97.74493049658622&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/694.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.44257332370017,-88.12039909850355&ll=44.44257332370017,-88.12039909850355&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/695.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-697">Voter 697's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 697 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;56'56"N 117&deg;46'53"W</div>
        <div class="text address"><i>approx.</i> 1099, Hillside Road, Chino Hills, San Bernardino County, California, 91709, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-698">Voter 698's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 698 voted for Trump</div> -->
        <div class="text coordinate"> 31&deg;17'56"N 92&deg;37'8"W</div>
        <div class="text address"><i>approx.</i> 866, Belgard Bend Road, Rapides Parish, Louisiana, 71409, United States</div>
        <div class="text gop precinct">Precinct Margin: R+62%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.949013,-117.781551&ll=33.949013,-117.781551&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/696.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=31.298931432927546,-92.61907354539558&ll=31.298931432927546,-92.61907354539558&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/697.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-699">Voter 699's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 699 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;12'56"N 97&deg;45'12"W</div>
        <div class="text address"><i>approx.</i> 1301, Irvin Street, Bridgeport, Wise County, Texas, 76426, United States</div>
        <div class="text gop precinct">Precinct Margin: R+53%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-700">Voter 700's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 700 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;34'23"N 121&deg;37'37"W</div>
        <div class="text address"><i>approx.</i> 208, Pine Canyon Road, Monterey County, California, 93908, United States</div>
        <div class="text gop precinct">Precinct Margin: R+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.215658,-97.753474&ll=33.215658,-97.753474&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/698.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.5730676,-121.6271363&ll=36.5730676,-121.6271363&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/699.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-701">Voter 701's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 701 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;31'13"N 70&deg;55'11"W</div>
        <div class="text address"><i>approx.</i> 24, Maple Street, Salem, Essex County, Massachusetts, 01970, United States</div>
        <div class="text dem precinct">Precinct Margin: D+41%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-702">Voter 702's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 702 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;28'59"N 81&deg;53'21"W</div>
        <div class="text address"><i>approx.</i> 2133, Rocky Step Road, Black Lick Estates, Putnam County, West Virginia, 25560, United States</div>
        <div class="text gop precinct">Precinct Margin: R+37%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.52035995,-70.91997129891354&ll=42.52035995,-70.91997129891354&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/700.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.483331447194395,-81.88935967974864&ll=38.483331447194395,-81.88935967974864&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/701.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-703">Voter 703's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 703 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;7'34"N 77&deg;28'27"W</div>
        <div class="text address"><i>approx.</i> 298, Birch Street, Flemington, Clinton County, Pennsylvania, 17745, United States</div>
        <div class="text gop precinct">Precinct Margin: R+27%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-704">Voter 704's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 704 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;0'51"N 88&deg;35'39"W</div>
        <div class="text address"><i>approx.</i> 2352, Abbey Avenue, Oshkosh, Winnebago County, Wisconsin, 54904, United States</div>
        <div class="text dem precinct">Precinct Margin: D+15%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.126134,-77.474388&ll=41.126134,-77.474388&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/702.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.014179840000004,-88.59422226666666&ll=44.014179840000004,-88.59422226666666&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/703.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-705">Voter 705's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 705 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;8'51"N 101&deg;53'20"W</div>
        <div class="text address"><i>approx.</i> 4806, Westway Trail, Amarillo, Randall County, Texas, 79109, United States</div>
        <div class="text gop precinct">Precinct Margin: R+49%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-706">Voter 706's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 706 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;21'52"N 122&deg;11'22"W</div>
        <div class="text address"><i>approx.</i> Coyote Trail, Palo Alto, Santa Clara County, California, 94028, United States</div>
        <div class="text dem precinct">Precinct Margin: D+73%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.147773,-101.88910947275413&ll=35.147773,-101.88910947275413&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/704.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.3647207,-122.1895449&ll=37.3647207,-122.1895449&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/705.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-707">Voter 707's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 707 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;10'30"N 111&deg;41'21"W</div>
        <div class="text address"><i>approx.</i> 2455, West Kiltie Lane, Equestrian Estates, Flagstaff, Coconino County, Arizona, 86005, United States</div>
        <div class="text dem precinct">Precinct Margin: D+30%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-708">Voter 708's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 708 voted for Trump</div> -->
        <div class="text coordinate"> 26&deg;15'34"N 98&deg;16'2"W</div>
        <div class="text address"><i>approx.</i> Sharyland North Junior High School, 5100, Dove Avenue, McAllen, Hidalgo County, Texas, 78504, United States</div>
        <div class="text dem precinct">Precinct Margin: D+13%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.1751319,-111.6892286&ll=35.1751319,-111.6892286&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/706.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.2596341,-98.2673270552633&ll=26.2596341,-98.2673270552633&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/707.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-709">Voter 709's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 709 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;27'44"N 121&deg;23'0"W</div>
        <div class="text address"><i>approx.</i> 8606, Delahye Circle, Elk Grove, Sacramento County, California, 95828, United States</div>
        <div class="text dem precinct">Precinct Margin: D+35%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-710">Voter 710's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 710 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;21'31"N 111&deg;42'29"W</div>
        <div class="text address"><i>approx.</i> 1338, 500 South Drive, Pleasant Grove, Utah County, Utah, 84062, United States</div>
        <div class="text gop precinct">Precinct Margin: R+44%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.462421322441294,-121.38346574644882&ll=38.462421322441294,-121.38346574644882&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/708.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.358628510204085,-111.7081957755102&ll=40.358628510204085,-111.7081957755102&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/709.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-711">Voter 711's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 711 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;11'38"N 97&deg;47'37"W</div>
        <div class="text address"><i>approx.</i> 7107, Cooper Lane, Austin, Travis County, Texas, 78745, United States</div>
        <div class="text dem precinct">Precinct Margin: D+65%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-712">Voter 712's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 712 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;19'29"N 97&deg;25'24"W</div>
        <div class="text address"><i>approx.</i> 14460, South Sooner Road, Oklahoma City, Cleveland County, Oklahoma, 73165, United States</div>
        <div class="text gop precinct">Precinct Margin: R+43%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.193961,-97.79362494315708&ll=30.193961,-97.79362494315708&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/710.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.32489389342806,-97.42351021136767&ll=35.32489389342806,-97.42351021136767&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/711.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-713">Voter 713's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 713 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;4'26"N 93&deg;15'7"W</div>
        <div class="text address"><i>approx.</i> 691, Marigold Terrace, Fridley, Anoka County, Minnesota, 55432, United States</div>
        <div class="text dem precinct">Precinct Margin: D+34%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-714">Voter 714's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 714 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;38'44"N 90&deg;39'34"W</div>
        <div class="text address"><i>approx.</i> 18018, Wild Horse Creek Road, Wildwood, Saint Louis County, Missouri, 63005, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.073990484414054,-93.25220317335452&ll=45.073990484414054,-93.25220317335452&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/712.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.64564771116698,-90.65966582719751&ll=38.64564771116698,-90.65966582719751&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/713.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-715">Voter 715's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 715 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;39'9"N 76&deg;57'43"W</div>
        <div class="text address"><i>approx.</i> 10159, Further Lane, Somerset, Waldorf, Charles County, Maryland, 20601, United States</div>
        <div class="text dem precinct">Precinct Margin: D+62%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-716">Voter 716's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 716 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;8'24"N 83&deg;52'42"W</div>
        <div class="text address"><i>approx.</i> 1927, North Black Road, Mason Township, Arenac County, Michigan, 48766, United States</div>
        <div class="text gop precinct">Precinct Margin: R+43%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.6525466624177,-76.96207890554237&ll=38.6525466624177,-76.96207890554237&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/714.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.140127325279046,-83.87841672643864&ll=44.140127325279046,-83.87841672643864&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/715.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-717">Voter 717's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 717 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;45'11"N 122&deg;14'52"W</div>
        <div class="text address"><i>approx.</i> Marsh Overlook, Alameda, Alameda County, California, 94501, United States</div>
        <div class="text dem precinct">Precinct Margin: D+69%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-718">Voter 718's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 718 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;6'16"N 82&deg;50'0"W</div>
        <div class="text address"><i>approx.</i> Greenwyck Crossing, Columbus, Sharon, Franklin County, Ohio, 43054, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.75326768222107,-122.24779348886473&ll=37.75326768222107,-122.24779348886473&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/716.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.104523830794655,-82.83340422903679&ll=40.104523830794655,-82.83340422903679&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/717.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-719">Voter 719's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 719 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;24'20"N 71&deg;42'48"W</div>
        <div class="text address"><i>approx.</i> White Trail, Charlestown, South County, Rhode Island, 02808, United States</div>
        <div class="text dem precinct">Precinct Margin: D+15%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-720">Voter 720's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 720 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;5'23"N 119&deg;34'11"W</div>
        <div class="text address"><i>approx.</i> 1846, Josephine Avenue, Corcoran, Kings County, California, 93212, United States</div>
        <div class="text dem precinct">Precinct Margin: D+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.4056707,-71.7134035&ll=41.4056707,-71.7134035&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/718.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.08979279591836,-119.56988953061224&ll=36.08979279591836,-119.56988953061224&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/719.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-721">Voter 721's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 721 voted for Biden</div> -->
        <div class="text coordinate"> 46&deg;40'2"N 120&deg;32'56"W</div>
        <div class="text address"><i>approx.</i> 1327, West Goodlander Road, Selah, Yakima County, Washington, 98942, United States</div>
        <div class="text gop precinct">Precinct Margin: R+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-722">Voter 722's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 722 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;31'49"N 94&deg;18'6"W</div>
        <div class="text address"><i>approx.</i> 25, Northwest 20th Road, Wimmer, Barton County, Missouri, 64759, United States</div>
        <div class="text gop precinct">Precinct Margin: R+80%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=46.667234114727826,-120.54909257501824&ll=46.667234114727826,-120.54909257501824&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/720.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.530370879820275,-94.30183424519029&ll=37.530370879820275,-94.30183424519029&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/721.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-723">Voter 723's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 723 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;59'16"N 118&deg;23'28"W</div>
        <div class="text address"><i>approx.</i> Hannum Avenue, Fox Hills, Culver City, Los Angeles County, California, 90230, United States</div>
        <div class="text dem precinct">Precinct Margin: D+68%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-724">Voter 724's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 724 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;23'2"N 78&deg;32'52"W</div>
        <div class="text address"><i>approx.</i> 202, West Hill Street, Benson, Johnston County, North Carolina, 27504, United States</div>
        <div class="text gop precinct">Precinct Margin: R+46%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.98801853298203,-118.39132855159903&ll=33.98801853298203,-118.39132855159903&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/722.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.383925928497085,-78.54781275323255&ll=35.383925928497085,-78.54781275323255&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/723.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-725">Voter 725's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 725 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;19'55"N 96&deg;10'24"W</div>
        <div class="text address"><i>approx.</i> North 165th Street, Summer Hill Farm, Douglas County, Nebraska, 68007, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-726">Voter 726's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 726 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;3'21"N 73&deg;51'33"W</div>
        <div class="text address"><i>approx.</i> 2, Carriage Trail, Greystone on Hudson, Village of Tarrytown, Town of Greenburgh, Westchester County, New York, 10591, United States</div>
        <div class="text dem precinct">Precinct Margin: D+43%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.33209733783505,-96.17337205144067&ll=41.33209733783505,-96.17337205144067&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/724.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.0558479,-73.859297&ll=41.0558479,-73.859297&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/725.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-727">Voter 727's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 727 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;31'2"N 74&deg;2'27"W</div>
        <div class="text address"><i>approx.</i> 89, Creek Run Road, Glenwood Park, Town of Newburgh, Orange County, New York, 12550, United States</div>
        <div class="text dem precinct">Precinct Margin: D+12%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-728">Voter 728's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 728 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;9'2"N 71&deg;14'50"W</div>
        <div class="text address"><i>approx.</i> 13, School Street, Walpole, Norfolk County, Massachusetts, 02081, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.517428,-74.0410392&ll=41.517428,-74.0410392&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/726.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.1507737,-71.2473924230826&ll=42.1507737,-71.2473924230826&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/727.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-729">Voter 729's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 729 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;21'23"N 122&deg;39'3"W</div>
        <div class="text address"><i>approx.</i> 23128, Bland Circle, Savannah Oaks, Willamette, West Linn, Clackamas County, Oregon, 97068, United States</div>
        <div class="text dem precinct">Precinct Margin: D+31%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-730">Voter 730's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 730 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;44'37"N 107&deg;13'32"W</div>
        <div class="text address"><i>approx.</i> I 25, Derry, Sierra County, New Mexico, 87933, United States</div>
        <div class="text gop precinct">Precinct Margin: R+2%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.35644575,-122.65088290730769&ll=45.35644575,-122.65088290730769&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/728.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.74379354812805,-107.22568660738003&ll=32.74379354812805,-107.22568660738003&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/729.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-731">Voter 731's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 731 voted for Biden</div> -->
        <div class="text coordinate"> 47&deg;35'3"N 122&deg;18'23"W</div>
        <div class="text address"><i>approx.</i> 2106, 20th Avenue South, Beacon Hill, Seattle, King County, Washington, 98144, United States</div>
        <div class="text dem precinct">Precinct Margin: D+63%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-732">Voter 732's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 732 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;58'1"N 90&deg;59'56"W</div>
        <div class="text address"><i>approx.</i> 99, Southwest Lane, Troy, Lincoln County, Missouri, 63379, United States</div>
        <div class="text gop precinct">Precinct Margin: R+51%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.584230399999996,-122.30648960821486&ll=47.584230399999996,-122.30648960821486&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/730.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.967028,-90.998918&ll=38.967028,-90.998918&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/731.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-733">Voter 733's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 733 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;47'8"N 73&deg;58'16"W</div>
        <div class="text address"><i>approx.</i> 38, West 86th Street, Upper West Side, Manhattan, New York County, City of New York, New York, 10024, United States</div>
        <div class="text dem precinct">Precinct Margin: D+79%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-734">Voter 734's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 734 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;50'16"N 93&deg;18'54"W</div>
        <div class="text address"><i>approx.</i> 9146, Upton Avenue South, Bloomington, Hennepin County, Minnesota, 55431, United States</div>
        <div class="text dem precinct">Precinct Margin: D+30%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.78582555,-73.97136545057276&ll=40.78582555,-73.97136545057276&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/732.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.83802644336167,-93.3151130095656&ll=44.83802644336167,-93.3151130095656&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/733.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-735">Voter 735's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 735 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;13'26"N 89&deg;46'40"W</div>
        <div class="text address"><i>approx.</i> Bayou Country General Store, East Howze Beach Road, Slidell, St. Tammany Parish, Louisiana, 70458, United States</div>
        <div class="text dem precinct">Precinct Margin: D+26%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-736">Voter 736's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 736 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;44'53"N 81&deg;36'36"W</div>
        <div class="text address"><i>approx.</i> 573, East Cardinal Circle, St. Marys, Camden County, Georgia, 31558, United States</div>
        <div class="text gop precinct">Precinct Margin: R+43%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.22405242626362,-89.77789917965062&ll=30.22405242626362,-89.77789917965062&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/734.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.748149407158202,-81.61007950586205&ll=30.748149407158202,-81.61007950586205&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/735.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-737">Voter 737's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 737 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;41'10"N 88&deg;10'38"W</div>
        <div class="text address"><i>approx.</i> 4431, Esquire Circle, River Run, Naperville, Will County, Illinois, 60564, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-738">Voter 738's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 738 voted for Trump</div> -->
        <div class="text coordinate"> 26&deg;32'24"N 81&deg;55'23"W</div>
        <div class="text address"><i>approx.</i> 6913, Deep Lagoon Lane, Lee County, Florida, 33919, United States</div>
        <div class="text gop precinct">Precinct Margin: R+24%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.686373700000004,-88.1774259554919&ll=41.686373700000004,-88.1774259554919&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/736.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.540100670498347,-81.9230801104649&ll=26.540100670498347,-81.9230801104649&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/737.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-739">Voter 739's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 739 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;33'3"N 122&deg;30'28"W</div>
        <div class="text address"><i>approx.</i> 3828, Northeast 149th Avenue, Wilkes, Portland, Multnomah County, Oregon, 97230, United States</div>
        <div class="text dem precinct">Precinct Margin: D+37%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-740">Voter 740's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 740 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;34'22"N 90&deg;35'1"W</div>
        <div class="text address"><i>approx.</i> 5027, Brown Street, Colony Park, Davenport, Scott County, Iowa, 52806, United States</div>
        <div class="text dem precinct">Precinct Margin: D+18%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.5509053,-122.50802123717557&ll=45.5509053,-122.50802123717557&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/738.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.57301011111111,-90.58364944444445&ll=41.57301011111111,-90.58364944444445&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/739.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-741">Voter 741's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 741 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;6'15"N 106&deg;32'22"W</div>
        <div class="text address"><i>approx.</i> 9412, Arvada Avenue Northeast, Mesa Village, Albuquerque, Bernalillo County, New Mexico, 87112, United States</div>
        <div class="text dem precinct">Precinct Margin: D+26%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-742">Voter 742's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 742 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;1'8"N 87&deg;47'12"W</div>
        <div class="text address"><i>approx.</i> 10276, 800 East Road, Vermilion County, Illinois, 61841, United States</div>
        <div class="text gop precinct">Precinct Margin: R+52%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.1044016597516,-106.53962593723051&ll=35.1044016597516,-106.53962593723051&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/740.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.01895518277265,-87.78670524843794&ll=40.01895518277265,-87.78670524843794&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/741.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-743">Voter 743's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 743 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;47'10"N 122&deg;24'15"W</div>
        <div class="text address"><i>approx.</i> Four Seasons, 757, Market Street, Union Square, San Francisco, California, 94103, United States</div>
        <div class="text dem precinct">Precinct Margin: D+52%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-744">Voter 744's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 744 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;11'47"N 76&deg;38'39"W</div>
        <div class="text address"><i>approx.</i> 20, Archwood Avenue, Shirley Estates, Woodlawn Heights, Anne Arundel County, Maryland, 21061, United States</div>
        <div class="text gop precinct">Precinct Margin: R+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.786215850000005,-122.40424732373828&ll=37.786215850000005,-122.40424732373828&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/742.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.196536,-76.644378&ll=39.196536,-76.644378&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/743.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-745">Voter 745's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 745 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;8'28"N 80&deg;13'17"W</div>
        <div class="text address"><i>approx.</i> 1382, Northwest 54th Avenue, Lauderhill, Sunrise, Broward County, Florida, 33313, United States</div>
        <div class="text dem precinct">Precinct Margin: D+88%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-746">Voter 746's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 746 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;53'54"N 93&deg;3'24"W</div>
        <div class="text address"><i>approx.</i> 509, 18th Avenue North, South St. Paul, Dakota County, Minnesota, 55075, United States</div>
        <div class="text dem precinct">Precinct Margin: D+21%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.141308187677065,-80.22154454169043&ll=26.141308187677065,-80.22154454169043&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/744.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.89834835,-93.05670976202258&ll=44.89834835,-93.05670976202258&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/745.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-747">Voter 747's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 747 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;3'37"N 80&deg;2'29"W</div>
        <div class="text address"><i>approx.</i> 359, Melvin Road, Proctor Heights, Summit Township, Erie County, Pennsylvania, 16509, United States</div>
        <div class="text gop precinct">Precinct Margin: R+10%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-748">Voter 748's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 748 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;9'19"N 121&deg;39'34"W</div>
        <div class="text address"><i>approx.</i> 10699, Crystal Springs Road, Klamath County, Oregon, 97603, United States</div>
        <div class="text gop precinct">Precinct Margin: R+72%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.06054971817303,-80.04143336768577&ll=42.06054971817303,-80.04143336768577&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/746.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.1554038,-121.6596492&ll=42.1554038,-121.6596492&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/747.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-749">Voter 749's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 749 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;50'19"N 94&deg;30'17"W</div>
        <div class="text address"><i>approx.</i> East 157th Street, Hardee Meadows, Belton, Cass County, Missouri, 64149, United States</div>
        <div class="text gop precinct">Precinct Margin: R+20%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-750">Voter 750's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 750 voted for Trump</div> -->
        <div class="text coordinate"> 28&deg;9'53"N 82&deg;21'58"W</div>
        <div class="text address"><i>approx.</i> 20359, Starfinder Way, Tampa, Hillsborough County, Florida, 33647, United States</div>
        <div class="text dem precinct">Precinct Margin: D+39%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.83880390829489,-94.50484761477975&ll=38.83880390829489,-94.50484761477975&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/748.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=28.16483184417849,-82.36614078066059&ll=28.16483184417849,-82.36614078066059&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/749.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-751">Voter 751's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 751 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;12'2"N 84&deg;33'25"W</div>
        <div class="text address"><i>approx.</i> 1950, Connecticut Avenue, College Hill, Cincinnati, Hamilton County, Ohio, 45224, United States</div>
        <div class="text dem precinct">Precinct Margin: D+64%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-752">Voter 752's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 752 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;56'21"N 82&deg;5'1"W</div>
        <div class="text address"><i>approx.</i> 909, Pine Mountain Road, Minpro, Mitchell County, North Carolina, 28777, United States</div>
        <div class="text gop precinct">Precinct Margin: R+55%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.20064685,-84.55722131422974&ll=39.20064685,-84.55722131422974&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/750.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.939427,-82.0837613&ll=35.939427,-82.0837613&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/751.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-753">Voter 753's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 753 voted for Biden</div> -->
        <div class="text coordinate"> 43&deg;0'35"N 78&deg;43'1"W</div>
        <div class="text address"><i>approx.</i> 101, Farmington Road, Woodstream Farms, Buffalo, Erie County, New York, 14221, United States</div>
        <div class="text dem precinct">Precinct Margin: D+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-754">Voter 754's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 754 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;12'3"N 96&deg;12'41"W</div>
        <div class="text address"><i>approx.</i> 5790, South 190th Terrace, Douglas County, Nebraska, 68135, United States</div>
        <div class="text gop precinct">Precinct Margin: R+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.0097386,-78.7170702&ll=43.0097386,-78.7170702&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/752.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.20102436839268,-96.21165761342051&ll=41.20102436839268,-96.21165761342051&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/753.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-755">Voter 755's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 755 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;31'58"N 122&deg;15'12"W</div>
        <div class="text address"><i>approx.</i> 564, Island Place, Redwood Shores, Redwood City, San Mateo County, California, 94065, United States</div>
        <div class="text dem precinct">Precinct Margin: D+55%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-756">Voter 756's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 756 voted for Trump</div> -->
        <div class="text coordinate"> 27&deg;3'31"N 82&deg;21'31"W</div>
        <div class="text address"><i>approx.</i> 498, Cardiff Road, Sarasota County, Florida, 34293, United States</div>
        <div class="text gop precinct">Precinct Margin: R+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.5329809,-122.25335706089425&ll=37.5329809,-122.25335706089425&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/754.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=27.0587432,-82.3586838&ll=27.0587432,-82.3586838&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/755.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-757">Voter 757's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 757 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;28'45"N 97&deg;29'34"W</div>
        <div class="text address"><i>approx.</i> The University of Oklahoma (Health Sciences Center), Northeast 13th Street, Oklahoma City, Oklahoma County, Oklahoma, 73117, United States</div>
        <div class="text dem precinct">Precinct Margin: D+49%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-758">Voter 758's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 758 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;50'15"N 86&deg;8'37"W</div>
        <div class="text address"><i>approx.</i> 4489, Carrollton Avenue, Watson Park, Meridian Kessler, Indianapolis, Marion County, Indiana, 46205, United States</div>
        <div class="text dem precinct">Precinct Margin: D+78%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.4791873,-97.49296051343218&ll=35.4791873,-97.49296051343218&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/756.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.837635222222225,-86.1436408888889&ll=39.837635222222225,-86.1436408888889&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/757.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-759">Voter 759's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 759 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;20'2"N 74&deg;10'57"W</div>
        <div class="text address"><i>approx.</i> 150, Franklin Avenue, Village of Monroe, Town of Monroe, Orange County, New York, 10950, United States</div>
        <div class="text dem precinct">Precinct Margin: D+3%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-760">Voter 760's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 760 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;49'22"N 84&deg;55'18"W</div>
        <div class="text address"><i>approx.</i> 347, Stevenson Street, Decatur, Adams County, Indiana, 46733, United States</div>
        <div class="text gop precinct">Precinct Margin: R+27%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.3339424,-74.1826675&ll=41.3339424,-74.1826675&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/758.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.82289777936117,-84.9219386133377&ll=40.82289777936117,-84.9219386133377&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/759.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-761">Voter 761's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 761 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;41'26"N 105&deg;56'58"W</div>
        <div class="text address"><i>approx.</i> 683, Jimenez Street, Santa Fe, Santa Fe County, New Mexico, 87501, United States</div>
        <div class="text dem precinct">Precinct Margin: D+84%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-762">Voter 762's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 762 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;6'42"N 97&deg;7'4"W</div>
        <div class="text address"><i>approx.</i> 920, South Murphy Street, Tradan Heights, Stillwater, Payne County, Oklahoma, 74074, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.6908305048123,-105.94969038043236&ll=35.6908305048123,-105.94969038043236&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/760.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.1118033,-97.117955&ll=36.1118033,-97.117955&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/761.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-763">Voter 763's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 763 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;14'1"N 118&deg;31'38"W</div>
        <div class="text address"><i>approx.</i> 18101, Osborne Street, Northridge, Los Angeles, Los Angeles County, California, 91325, United States</div>
        <div class="text dem precinct">Precinct Margin: D+42%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-764">Voter 764's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 764 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;14'25"N 89&deg;30'39"W</div>
        <div class="text address"><i>approx.</i> Fair Oaks Golf Course, Wirt Road, Oakland, Fayette County, Tennessee, 38060, United States</div>
        <div class="text gop precinct">Precinct Margin: R+38%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.233784,-118.527495&ll=34.233784,-118.527495&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/762.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.24029805,-89.51109047540544&ll=35.24029805,-89.51109047540544&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/763.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-765">Voter 765's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 765 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;25'29"N 83&deg;6'28"W</div>
        <div class="text address"><i>approx.</i> 461, Erle Street, Grixdale Farms, Detroit, Wayne County, Michigan, 48203, United States</div>
        <div class="text dem precinct">Precinct Margin: D+87%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-766">Voter 766's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 766 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;51'11"N 93&deg;26'52"W</div>
        <div class="text address"><i>approx.</i> 13444, Winchester Place, Eden Prairie, Hennepin County, Minnesota, 55344, United States</div>
        <div class="text dem precinct">Precinct Margin: D+41%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.42482219084056,-83.10785706869852&ll=42.42482219084056,-83.10785706869852&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/764.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.85323,-93.447807&ll=44.85323,-93.447807&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/765.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-767">Voter 767's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 767 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;52'56"N 87&deg;48'8"W</div>
        <div class="text address"><i>approx.</i> 352, Wisconsin Avenue, Samuel A Rothermel Houses, Oak Park, Cook County, Illinois, 60302, United States</div>
        <div class="text dem precinct">Precinct Margin: D+78%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-768">Voter 768's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 768 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;44'55"N 91&deg;35'51"W</div>
        <div class="text address"><i>approx.</i> Sugar Bottom Bikes, 325, North Dubuque Street, North Liberty, Johnson County, Iowa, 52317, United States</div>
        <div class="text dem precinct">Precinct Margin: D+31%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.882300277157086,-87.80228535821772&ll=41.882300277157086,-87.80228535821772&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/766.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.74865715,-91.5977227186676&ll=41.74865715,-91.5977227186676&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/767.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-769">Voter 769's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 769 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;32'53"N 106&deg;55'41"W</div>
        <div class="text address"><i>approx.</i> 447, North Wisconsin Street, Gunnison, Gunnison County, Colorado, 81230, United States</div>
        <div class="text dem precinct">Precinct Margin: D+40%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-770">Voter 770's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 770 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;2'36"N 86&deg;2'16"W</div>
        <div class="text address"><i>approx.</i> 823, Hinds Road, The Highlands, Gadsden, Etowah County, Alabama, 35904, United States</div>
        <div class="text gop precinct">Precinct Margin: R+50%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.54825175510879,-106.92830346057444&ll=38.54825175510879,-106.92830346057444&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/768.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.043442,-86.0380168&ll=34.043442,-86.0380168&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/769.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-771">Voter 771's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 771 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;57'53"N 74&deg;58'54"W</div>
        <div class="text address"><i>approx.</i> 698, Grand Avenue, Lenola, Moorestown Township, Burlington County, New Jersey, 08057, United States</div>
        <div class="text dem precinct">Precinct Margin: D+23%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-772">Voter 772's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 772 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;0'25"N 73&deg;42'36"W</div>
        <div class="text address"><i>approx.</i> 50, Putnam Road, Wayville, Town of Stillwater, Town of Saratoga, Saratoga County, New York, 12170, United States</div>
        <div class="text gop precinct">Precinct Margin: R+16%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.964732,-74.981792&ll=39.964732,-74.981792&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/770.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.007167455919344,-73.71019047815926&ll=43.007167455919344,-73.71019047815926&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/771.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-773">Voter 773's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 773 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;51'39"N 88&deg;6'57"W</div>
        <div class="text address"><i>approx.</i> 557, South Carlton Avenue, Wheaton, DuPage County, Illinois, 60187, United States</div>
        <div class="text dem precinct">Precinct Margin: D+32%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-774">Voter 774's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 774 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;9'32"N 96&deg;0'24"W</div>
        <div class="text address"><i>approx.</i> 2120, Leigh Street, Papillion, Sarpy County, Nebraska, 68133, United States</div>
        <div class="text gop precinct">Precinct Margin: R+13%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.861054424242425,-88.11606881818182&ll=41.861054424242425,-88.11606881818182&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/772.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.15912758083496,-96.00686417522992&ll=41.15912758083496,-96.00686417522992&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/773.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-775">Voter 775's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 775 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;51'47"N 93&deg;38'10"W</div>
        <div class="text address"><i>approx.</i> 2747, 130th Street, Wright County, Iowa, 50421, United States</div>
        <div class="text gop precinct">Precinct Margin: R+26%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-776">Voter 776's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 776 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;16'5"N 88&deg;11'12"W</div>
        <div class="text address"><i>approx.</i> East State Road, Island Lake, Lake County, Illinois, 60042, United States</div>
        <div class="text dem precinct">Precinct Margin: D+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.86318627336871,-93.63615971999204&ll=42.86318627336871,-93.63615971999204&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/774.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.26816735417416,-88.18673384455047&ll=42.26816735417416,-88.18673384455047&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/775.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-777">Voter 777's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 777 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;3'10"N 91&deg;29'29"W</div>
        <div class="text address"><i>approx.</i> 5549, Old Springfield Road, Crawford County, Missouri, 65453, United States</div>
        <div class="text gop precinct">Precinct Margin: R+61%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-778">Voter 778's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 778 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;13'46"N 122&deg;35'53"W</div>
        <div class="text address"><i>approx.</i> 77, Cader Lane, Petaluma, Sonoma County, California, 94954, United States</div>
        <div class="text dem precinct">Precinct Margin: D+56%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.05304072212801,-91.49140096634041&ll=38.05304072212801,-91.49140096634041&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/776.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.229475,-122.5982594&ll=38.229475,-122.5982594&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/777.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-779">Voter 779's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 779 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;49'7"N 75&deg;8'1"W</div>
        <div class="text address"><i>approx.</i> 601, South Barlow Avenue, Deptford, Deptford Township, Gloucester County, New Jersey, 08096, United States</div>
        <div class="text dem precinct">Precinct Margin: D+13%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-780">Voter 780's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 780 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;40'17"N 84&deg;58'59"W</div>
        <div class="text address"><i>approx.</i> 154, Martin Road, Carroll County, Georgia, 30116, United States</div>
        <div class="text gop precinct">Precinct Margin: R+45%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.818815,-75.133672&ll=39.818815,-75.133672&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/778.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.67146168645518,-84.98324225982564&ll=33.67146168645518,-84.98324225982564&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/779.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-781">Voter 781's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 781 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;48'51"N 116&deg;55'52"W</div>
        <div class="text address"><i>approx.</i> 1346, Groveland Terrace, Bostonia, El Cajon, San Diego County, California, 92021, United States</div>
        <div class="text gop precinct">Precinct Margin: R+13%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-782">Voter 782's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 782 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;19'17"N 96&deg;9'31"W</div>
        <div class="text address"><i>approx.</i> Gilbert Boarding Kennels, 6904, North 156th Street, Bennington, Douglas County, Nebraska, 68007, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.814437,-116.931142&ll=32.814437,-116.931142&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/780.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.3215684,-96.15877300706472&ll=41.3215684,-96.15877300706472&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/781.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-783">Voter 783's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 783 voted for Biden</div> -->
        <div class="text coordinate"> 27&deg;22'3"N 82&deg;30'38"W</div>
        <div class="text address"><i>approx.</i> 3705, Voorne Street, Beverley Terrace, Sarasota County, Florida, 34234, United States</div>
        <div class="text gop precinct">Precinct Margin: R+15%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-784">Voter 784's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 784 voted for Trump</div> -->
        <div class="text coordinate"> 31&deg;22'11"N 84&deg;55'48"W</div>
        <div class="text address"><i>approx.</i> 253, Lancelot Lane, Blakely, Early County, Georgia, 39823, United States</div>
        <div class="text dem precinct">Precinct Margin: D+27%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=27.367739,-82.510674&ll=27.367739,-82.510674&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/782.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=31.369989021777847,-84.93006701266813&ll=31.369989021777847,-84.93006701266813&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/783.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-785">Voter 785's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 785 voted for Biden</div> -->
        <div class="text coordinate"> 47&deg;32'20"N 122&deg;16'13"W</div>
        <div class="text address"><i>approx.</i> Myrtle Street Apartments, South Myrtle Street, Brighton Beach, Seattle, King County, Washington, 98118, United States</div>
        <div class="text dem precinct">Precinct Margin: D+69%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-786">Voter 786's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 786 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;20'39"N 92&deg;39'26"W</div>
        <div class="text address"><i>approx.</i> 897, Hayes City Road, Union County, Arkansas, 71762, United States</div>
        <div class="text gop precinct">Precinct Margin: R+66%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.53915225,-122.27045572342465&ll=47.53915225,-122.27045572342465&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/784.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.34438379273318,-92.65727668977088&ll=33.34438379273318,-92.65727668977088&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/785.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-787">Voter 787's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 787 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;40'16"N 121&deg;46'33"W</div>
        <div class="text address"><i>approx.</i> 732, Dingle Lane, Woodland, Yolo County, California, 95695, United States</div>
        <div class="text dem precinct">Precinct Margin: D+18%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-788">Voter 788's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 788 voted for Trump</div> -->
        <div class="text coordinate"> 25&deg;41'6"N 80&deg;28'12"W</div>
        <div class="text address"><i>approx.</i> Southwest 88th Street, Miami-Dade County, Florida, 33176, United States</div>
        <div class="text gop precinct">Precinct Margin: R+5%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.67137490527371,-121.77605522575266&ll=38.67137490527371,-121.77605522575266&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/786.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=25.68508442234574,-80.47024330485576&ll=25.68508442234574,-80.47024330485576&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/787.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-789">Voter 789's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 789 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;42'54"N 73&deg;57'42"W</div>
        <div class="text address"><i>approx.</i> 136, North 1st Street, Brooklyn, Kings County, City of New York, New York, 11249, United States</div>
        <div class="text dem precinct">Precinct Margin: D+77%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-790">Voter 790's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 790 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;13'33"N 86&deg;55'1"W</div>
        <div class="text address"><i>approx.</i> Coalmont Road, Coalmont, Helena, Shelby County, Alabama, 35080, United States</div>
        <div class="text gop precinct">Precinct Margin: R+39%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.71511425,-73.96171985478571&ll=40.71511425,-73.96171985478571&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/788.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.2260247519539,-86.91718857110087&ll=33.2260247519539,-86.91718857110087&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/789.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-791">Voter 791's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 791 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;57'3"N 76&deg;46'3"W</div>
        <div class="text address"><i>approx.</i> Warners Discovery Way, Fairwood, Prince George's County, Maryland, United States</div>
        <div class="text dem precinct">Precinct Margin: D+84%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-792">Voter 792's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 792 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;17'12"N 83&deg;58'52"W</div>
        <div class="text address"><i>approx.</i> South Wright Street, Blanchester, Clinton County, Ohio, 45107, United States</div>
        <div class="text gop precinct">Precinct Margin: R+66%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.95086835,-76.76774029622156&ll=38.95086835,-76.76774029622156&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/790.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.28683832991266,-83.9812526176117&ll=39.28683832991266,-83.9812526176117&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/791.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-793">Voter 793's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 793 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;47'49"N 78&deg;31'45"W</div>
        <div class="text address"><i>approx.</i> Coffey Street, Green Pines, Milburnie, Wake County, North Carolina, 27604, United States</div>
        <div class="text dem precinct">Precinct Margin: D+36%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-794">Voter 794's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 794 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;33'42"N 71&deg;1'38"W</div>
        <div class="text address"><i>approx.</i> 5, Surrey Lane, Peabody, Essex County, Massachusetts, 01949, United States</div>
        <div class="text gop precinct">Precinct Margin: R+3%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.7972161,-78.5292201&ll=35.7972161,-78.5292201&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/792.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.561802400000005,-71.02728146674733&ll=42.561802400000005,-71.02728146674733&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/793.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-795">Voter 795's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 795 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;11'25"N 73&deg;16'2"W</div>
        <div class="text address"><i>approx.</i> 381, Hollydale Road, Fairfield, Fairfield County, Connecticut, 06824, United States</div>
        <div class="text dem precinct">Precinct Margin: D+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-796">Voter 796's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 796 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;23'14"N 93&deg;8'19"W</div>
        <div class="text address"><i>approx.</i> 537, Cottonwood Street, Fair Grove, Greene County, Missouri, 65648, United States</div>
        <div class="text gop precinct">Precinct Margin: R+52%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.1903885,-73.2673751&ll=41.1903885,-73.2673751&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/794.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.38729641745089,-93.13883348683827&ll=37.38729641745089,-93.13883348683827&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/795.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-797">Voter 797's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 797 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;8'27"N 117&deg;4'19"W</div>
        <div class="text address"><i>approx.</i> 1219, Armstrong Circle, Escondido, San Diego County, California, 92027, United States</div>
        <div class="text dem precinct">Precinct Margin: D+35%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-798">Voter 798's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 798 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;56'10"N 89&deg;57'27"W</div>
        <div class="text address"><i>approx.</i> 5298, Iraon Circle, Southaven, DeSoto County, Mississippi, 38671, United States</div>
        <div class="text gop precinct">Precinct Margin: R+30%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.140923,-117.072064&ll=33.140923,-117.072064&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/796.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.936144,-89.957706&ll=34.936144,-89.957706&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/797.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-799">Voter 799's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 799 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;9'32"N 96&deg;0'24"W</div>
        <div class="text address"><i>approx.</i> 2120, Leigh Street, Papillion, Sarpy County, Nebraska, 68133, United States</div>
        <div class="text gop precinct">Precinct Margin: R+13%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-800">Voter 800's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 800 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;52'52"N 88&deg;1'15"W</div>
        <div class="text address"><i>approx.</i> 199, West Willow Street, Lombard, DuPage County, Illinois, 60148, United States</div>
        <div class="text dem precinct">Precinct Margin: D+28%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.15912758083496,-96.00686417522992&ll=41.15912758083496,-96.00686417522992&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/798.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.881168,-88.020944&ll=41.881168,-88.020944&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/799.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-801">Voter 801's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 801 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;2'27"N 75&deg;24'16"W</div>
        <div class="text address"><i>approx.</i> Warren Filipone Memorial Park Tennis Courts, South Devon Avenue, West Wayne, Wayne, Radnor Township, Delaware County, Pennsylvania, 19087-3305, United States</div>
        <div class="text dem precinct">Precinct Margin: D+38%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-802">Voter 802's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 802 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;37'48"N 70&deg;38'45"W</div>
        <div class="text address"><i>approx.</i> 12, Heritage Way, Gloucester, Essex County, Massachusetts, 01930-3540, United States</div>
        <div class="text dem precinct">Precinct Margin: D+37%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.0410811,-75.40447175&ll=40.0410811,-75.40447175&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/800.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.63022185,-70.6458852627614&ll=42.63022185,-70.6458852627614&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/801.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-803">Voter 803's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 803 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;10'14"N 105&deg;5'27"W</div>
        <div class="text address"><i>approx.</i> Kensington Ditch Trail, Longmont, Boulder County, Colorado, 80501-4919, United States</div>
        <div class="text dem precinct">Precinct Margin: D+45%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-804">Voter 804's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 804 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;3'24"N 76&deg;59'9"W</div>
        <div class="text address"><i>approx.</i> Jackson Road, East Springbrook, White Oak, Montgomery County, Maryland, 20904, United States</div>
        <div class="text dem precinct">Precinct Margin: D+67%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.1705963,-105.090991&ll=40.1705963,-105.090991&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/802.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.056686868967844,-76.98588656230459&ll=39.056686868967844,-76.98588656230459&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/803.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-805">Voter 805's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 805 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;11'19"N 97&deg;7'3"W</div>
        <div class="text address"><i>approx.</i> 1881, Concord Lane, Denton, Denton County, Texas, 76205, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-806">Voter 806's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 806 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;41'32"N 121&deg;47'13"W</div>
        <div class="text address"><i>approx.</i> 69, West Kentucky Avenue, Woodland, Yolo County, California, 95695, United States</div>
        <div class="text dem precinct">Precinct Margin: D+27%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.18888491870065,-97.11767629175142&ll=33.18888491870065,-97.11767629175142&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/804.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.69235105,-121.7870555788488&ll=38.69235105,-121.7870555788488&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/805.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-807">Voter 807's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 807 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;53'18"N 73&deg;7'16"W</div>
        <div class="text address"><i>approx.</i> 25, Annandale Road, Suffolk County, New York, 11790, United States</div>
        <div class="text dem precinct">Precinct Margin: D+11%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-808">Voter 808's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 808 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;59'38"N 81&deg;7'29"W</div>
        <div class="text address"><i>approx.</i> 941, Gatewood Road, East Oak Hill, Oak Hill, Fayette County, West Virginia, 25840, United States</div>
        <div class="text gop precinct">Precinct Margin: R+49%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.8883929,-73.12135170341615&ll=40.8883929,-73.12135170341615&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/806.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.99396398337923,-81.1248613466007&ll=37.99396398337923,-81.1248613466007&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/807.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-809">Voter 809's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 809 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;35'12"N 123&deg;15'54"W</div>
        <div class="text address"><i>approx.</i> Linus Pauling Middle School, Northwest Cleveland Avenue, Corvallis, Benton County, Oregon, 97330, United States</div>
        <div class="text dem precinct">Precinct Margin: D+62%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-810">Voter 810's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 810 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;54'14"N 79&deg;49'14"W</div>
        <div class="text address"><i>approx.</i> 2318, Kings Gate Lane, Mount Pleasant, Charleston County, South Carolina, 29466, United States</div>
        <div class="text gop precinct">Precinct Margin: R+25%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.586829699999996,-123.26511944987155&ll=44.586829699999996,-123.26511944987155&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/808.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.904136149405616,-79.82064334949118&ll=32.904136149405616,-79.82064334949118&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/809.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-811">Voter 811's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 811 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;6'4"N 80&deg;12'40"W</div>
        <div class="text address"><i>approx.</i> 1561, Southwest 46th Avenue, Fern Crest Village, Broadview Park, Sunrise, Broward County, Florida, 33317, United States</div>
        <div class="text dem precinct">Precinct Margin: D+13%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-812">Voter 812's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 812 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;2'9"N 86&deg;1'10"W</div>
        <div class="text address"><i>approx.</i> 399, Chestnut Street, Noblesville, Hamilton County, Indiana, 46060, United States</div>
        <div class="text gop precinct">Precinct Margin: R+28%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.10129,-80.211341&ll=26.10129,-80.211341&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/810.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.035938,-86.019615&ll=40.035938,-86.019615&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/811.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-813">Voter 813's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 813 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;49'47"N 93&deg;15'11"W</div>
        <div class="text address"><i>approx.</i> 1526, East 96th Street, Bloomington, Hennepin County, Minnesota, 55425, United States</div>
        <div class="text dem precinct">Precinct Margin: D+18%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-814">Voter 814's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 814 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;52'27"N 84&deg;4'19"W</div>
        <div class="text address"><i>approx.</i> 2573, West Cadmus Road, Madison Township, Lenawee County, Michigan, 49221, United States</div>
        <div class="text gop precinct">Precinct Margin: R+41%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.829993510638296,-93.25332989361702&ll=44.829993510638296,-93.25332989361702&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/812.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.87430991782165,-84.07207684829551&ll=41.87430991782165,-84.07207684829551&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/813.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-815">Voter 815's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 815 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;34'34"N 104&deg;53'12"W</div>
        <div class="text address"><i>approx.</i> 8745, East Kettle Place, Centennial, Arapahoe County, Colorado, 80112, United States</div>
        <div class="text dem precinct">Precinct Margin: D+28%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-816">Voter 816's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 816 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;53'51"N 123&deg;6'41"W</div>
        <div class="text address"><i>approx.</i> River Road South, Salem, Marion County, Oregon, United States</div>
        <div class="text dem precinct">Precinct Margin: D+26%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.57629708444558,-104.88675408686649&ll=39.57629708444558,-104.88675408686649&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/814.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.89772914959241,-123.11166654441105&ll=44.89772914959241,-123.11166654441105&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/815.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-817">Voter 817's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 817 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;13'51"N 72&deg;19'40"W</div>
        <div class="text address"><i>approx.</i> Herman Covey Wildlife Management Area, Bondsville Road, Ware, Hampshire County, Massachusetts, 01009, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-818">Voter 818's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 818 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;40'22"N 94&deg;45'34"W</div>
        <div class="text address"><i>approx.</i> Pauline Avenue, Polk County, Texas, United States</div>
        <div class="text gop precinct">Precinct Margin: R+72%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.2311052,-72.32804359899791&ll=42.2311052,-72.32804359899791&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/816.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.672934672642874,-94.75946361252855&ll=30.672934672642874,-94.75946361252855&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/817.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-819">Voter 819's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 819 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;51'1"N 79&deg;10'51"W</div>
        <div class="text address"><i>approx.</i> 191, Wild Rose Lane, Town of Pittsboro, Chatham County, North Carolina, 27312, United States</div>
        <div class="text dem precinct">Precinct Margin: D+45%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-820">Voter 820's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 820 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;35'48"N 76&deg;54'5"W</div>
        <div class="text address"><i>approx.</i> 2610, Shiloh Road, Shiloh, Carroll County, Maryland, 21074, United States</div>
        <div class="text gop precinct">Precinct Margin: R+38%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.8503948,-79.1810486&ll=35.8503948,-79.1810486&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/818.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.59680004703525,-76.90150782661202&ll=39.59680004703525,-76.90150782661202&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/819.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-821">Voter 821's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 821 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;10'45"N 77&deg;5'43"W</div>
        <div class="text address"><i>approx.</i> Our House Residential Job Training Center For Youth, Olney Laytonsville Road, Olney, Montgomery County, Maryland, 20832, United States</div>
        <div class="text dem precinct">Precinct Margin: D+27%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-822">Voter 822's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 822 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;8'51"N 75&deg;45'36"W</div>
        <div class="text address"><i>approx.</i> Mount Olive Cemetery, Revells Neck Road, Somerset County, Maryland, 21890, United States</div>
        <div class="text gop precinct">Precinct Margin: R+58%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.17923175,-77.09550286826021&ll=39.17923175,-77.09550286826021&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/820.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.1476174,-75.76018198017792&ll=38.1476174,-75.76018198017792&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/821.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-823">Voter 823's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 823 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;58'47"N 83&deg;42'31"W</div>
        <div class="text address"><i>approx.</i> 1343, Lynton Avenue, Flint, Genesee County, Michigan, 48507, United States</div>
        <div class="text dem precinct">Precinct Margin: D+14%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-824">Voter 824's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 824 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;47'21"N 97&deg;36'11"W</div>
        <div class="text address"><i>approx.</i> N3022 Road, Garvin County, Oklahoma, United States</div>
        <div class="text gop precinct">Precinct Margin: R+80%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.97976855033557,-83.70886589932886&ll=42.97976855033557,-83.70886589932886&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/822.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.789431,-97.603225&ll=34.789431,-97.603225&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/823.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-825">Voter 825's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 825 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;12'38"N 84&deg;35'56"W</div>
        <div class="text address"><i>approx.</i> 3533, Blue Rock Road, White Oak, Colerain Township, Hamilton County, Ohio, 45247, United States</div>
        <div class="text gop precinct">Precinct Margin: R+5%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-826">Voter 826's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 826 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;23'12"N 80&deg;41'35"W</div>
        <div class="text address"><i>approx.</i> 598, Pitts School Road Northwest, Riverwalk, Concord, Cabarrus County, North Carolina, 28027, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.210691249999996,-84.59908837217537&ll=39.210691249999996,-84.59908837217537&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/824.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.386883,-80.693325&ll=35.386883,-80.693325&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/825.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-827">Voter 827's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 827 voted for Biden</div> -->
        <div class="text coordinate"> 28&deg;55'56"N 81&deg;45'14"W</div>
        <div class="text address"><i>approx.</i> 11914, Lizard Lane, Lake County, Florida, 32784, United States</div>
        <div class="text gop precinct">Precinct Margin: R+36%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-828">Voter 828's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 828 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;18'37"N 122&deg;49'43"W</div>
        <div class="text address"><i>approx.</i> The Church of Jesus Christ of Latter-day Saints, 2900, Juanipero Way, Medford, Jackson County, Oregon, 97504, United States</div>
        <div class="text dem precinct">Precinct Margin: D+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=28.93246,-81.75412914173228&ll=28.93246,-81.75412914173228&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/826.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.31028035,-122.82880430760125&ll=42.31028035,-122.82880430760125&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/827.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-829">Voter 829's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 829 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;10'14"N 77&deg;37'47"W</div>
        <div class="text address"><i>approx.</i> Dixon Lane, Spotsylvania County, Virginia, 22551, United States</div>
        <div class="text gop precinct">Precinct Margin: R+15%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-830">Voter 830's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 830 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;10'2"N 83&deg;23'57"W</div>
        <div class="text address"><i>approx.</i> Albert Lane, New Boston, Huron Township, Wayne County, Michigan, United States</div>
        <div class="text gop precinct">Precinct Margin: R+30%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.1706767,-77.6297915&ll=38.1706767,-77.6297915&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/828.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.1674363,-83.3992557&ll=42.1674363,-83.3992557&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/829.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-831">Voter 831's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 831 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;0'5"N 84&deg;39'6"W</div>
        <div class="text address"><i>approx.</i> 516, West McCoy Road, Gaylord, Bagley Township, Otsego County, Michigan, 49735, United States</div>
        <div class="text gop precinct">Precinct Margin: R+36%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-832">Voter 832's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 832 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;24'7"N 86&deg;52'46"W</div>
        <div class="text address"><i>approx.</i> 1593, Melton Road, Shannon, Hoover, Jefferson County, Alabama, 35022, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.001586057236004,-84.65188350293134&ll=45.001586057236004,-84.65188350293134&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/830.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.402031958399284,-86.87966253495244&ll=33.402031958399284,-86.87966253495244&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/831.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-833">Voter 833's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 833 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;54'53"N 74&deg;2'23"W</div>
        <div class="text address"><i>approx.</i> Wiltwyck Golf Club, 404, Stewart Lane, City of Kingston, Town of Ulster, Ulster County, New York, 12401, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-834">Voter 834's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 834 voted for Trump</div> -->
        <div class="text coordinate"> 45&deg;12'51"N 93&deg;15'24"W</div>
        <div class="text address"><i>approx.</i> 563, 134th Lane Northeast, Ham Lake, Anoka County, Minnesota, 55304, United States</div>
        <div class="text gop precinct">Precinct Margin: R+23%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.91483795,-74.03992592417484&ll=41.91483795,-74.03992592417484&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/832.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.21430060096092,-93.25678459436615&ll=45.21430060096092,-93.25678459436615&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/833.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-835">Voter 835's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 835 voted for Biden</div> -->
        <div class="text coordinate"> 43&deg;9'1"N 76&deg;20'36"W</div>
        <div class="text address"><i>approx.</i> 111, Pebblewood Ln, Village of Baldwinsville, Town of Van Buren, Onondaga County, New York, 13027, United States</div>
        <div class="text dem precinct">Precinct Margin: D+15%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-836">Voter 836's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 836 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;18'13"N 79&deg;58'1"W</div>
        <div class="text address"><i>approx.</i> 4882, Ring Road Northwest, Fairland, Roanoke, Virginia, 24012, United States</div>
        <div class="text dem precinct">Precinct Margin: D+31%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.1503997,-76.34352574866011&ll=43.1503997,-76.34352574866011&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/834.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.30385439711703,-79.96699513874843&ll=37.30385439711703,-79.96699513874843&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/835.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-837">Voter 837's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 837 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;15'48"N 118&deg;27'7"W</div>
        <div class="text address"><i>approx.</i> 14674, Kingsbury Street, Los Angeles, Los Angeles County, California, 91345, United States</div>
        <div class="text dem precinct">Precinct Margin: D+47%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-838">Voter 838's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 838 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;22'28"N 117&deg;12'56"W</div>
        <div class="text address"><i>approx.</i> 552, Tumble Creek Terrace, Fallbrook, San Diego County, California, 92028, United States</div>
        <div class="text gop precinct">Precinct Margin: R+18%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.26344967677285,-118.45210839582016&ll=34.26344967677285,-118.45210839582016&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/836.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.374629,-117.215818&ll=33.374629,-117.215818&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/837.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-839">Voter 839's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 839 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;53'9"N 97&deg;57'23"W</div>
        <div class="text address"><i>approx.</i> 999, North Loop Street, San Marcos, Hays County, Texas, 78666, United States</div>
        <div class="text dem precinct">Precinct Margin: D+46%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-840">Voter 840's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 840 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;42'0"N 117&deg;57'20"W</div>
        <div class="text address"><i>approx.</i> Orange Coast Memorial Medical Center, 18111, Brookhurst Street, Fountain Valley, Orange County, California, 92708, United States</div>
        <div class="text dem precinct">Precinct Margin: D+3%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.885942609677382,-97.95661579392906&ll=29.885942609677382,-97.95661579392906&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/838.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.70010835,-117.95568247440247&ll=33.70010835,-117.95568247440247&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/839.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-841">Voter 841's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 841 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;37'59"N 83&deg;46'31"W</div>
        <div class="text address"><i>approx.</i> 2531, Powersville Road, Powersville, Peach County, Georgia, 31008, United States</div>
        <div class="text gop precinct">Precinct Margin: R+25%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-842">Voter 842's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 842 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;45'29"N 117&deg;47'43"W</div>
        <div class="text address"><i>approx.</i> 1291, Banbury Cross Road, Lemon Heights, North Tustin, Orange, Orange County, California, 92705, United States</div>
        <div class="text gop precinct">Precinct Margin: R+13%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.63324256662155,-83.77536494464161&ll=32.63324256662155,-83.77536494464161&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/840.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.758283000000006,-117.79539299261307&ll=33.758283000000006,-117.79539299261307&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/841.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-843">Voter 843's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 843 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;42'19"N 73&deg;48'14"W</div>
        <div class="text address"><i>approx.</i> 88-02, 153rd Street, Jamaica, Queens, City of New York, New York, 11432, United States</div>
        <div class="text dem precinct">Precinct Margin: D+79%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-844">Voter 844's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 844 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;53'44"N 87&deg;38'17"W</div>
        <div class="text address"><i>approx.</i> Superior House, 366, West Superior Street, River North, Near North Side, Chicago, Cook County, Illinois, 60654, United States</div>
        <div class="text dem precinct">Precinct Margin: D+50%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.7053893,-73.8039833&ll=40.7053893,-73.8039833&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/842.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.8958163,-87.6382165242373&ll=41.8958163,-87.6382165242373&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/843.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-845">Voter 845's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 845 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;34'48"N 93&deg;48'18"W</div>
        <div class="text address"><i>approx.</i> 860, 71st Street, Reed's Crossing, West Des Moines, Dallas County, Iowa, 50266, United States</div>
        <div class="text dem precinct">Precinct Margin: D+11%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-846">Voter 846's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 846 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;32'0"N 75&deg;59'10"W</div>
        <div class="text address"><i>approx.</i> Lake Wilderness Road, Wyoming County, Pennsylvania, 18657, United States</div>
        <div class="text gop precinct">Precinct Margin: R+30%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.5802216,-93.8051706&ll=41.5802216,-93.8051706&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/844.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.533368,-75.986221&ll=41.533368,-75.986221&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/845.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-847">Voter 847's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 847 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;23'14"N 122&deg;29'6"W</div>
        <div class="text address"><i>approx.</i> 3316, Hunter Road, Shasta County, California, 96022, United States</div>
        <div class="text gop precinct">Precinct Margin: R+53%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-848">Voter 848's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 848 voted for Trump</div> -->
        <div class="text coordinate"> 44&deg;49'26"N 122&deg;58'58"W</div>
        <div class="text address"><i>approx.</i> 8771, Parrish Gap Road Southeast, Marion County, Oregon, 97392, United States</div>
        <div class="text gop precinct">Precinct Margin: R+33%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.38724765534701,-122.48522810180982&ll=40.38724765534701,-122.48522810180982&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/846.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.82392857657892,-122.98303005647553&ll=44.82392857657892,-122.98303005647553&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/847.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-849">Voter 849's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 849 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;43'16"N 74&deg;12'40"W</div>
        <div class="text address"><i>approx.</i> University High School, Millington Avenue, Newark, Essex County, New Jersey, 07108, United States</div>
        <div class="text dem precinct">Precinct Margin: D+90%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-850">Voter 850's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 850 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;55'35"N 118&deg;15'28"W</div>
        <div class="text address"><i>approx.</i> 954, East 118th Street, Green Meadows, Los Angeles, Los Angeles County, California, 90059, United States</div>
        <div class="text dem precinct">Precinct Margin: D+70%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.72123055,-74.21133429727769&ll=40.72123055,-74.21133429727769&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/848.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.926570818181816,-118.25796718181819&ll=33.926570818181816,-118.25796718181819&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/849.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-851">Voter 851's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 851 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;14'55"N 83&deg;38'28"W</div>
        <div class="text address"><i>approx.</i> 1850, Whittier Road, College Heights, Ypsilanti, Washtenaw County, Michigan, 48197, United States</div>
        <div class="text dem precinct">Precinct Margin: D+69%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-852">Voter 852's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 852 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;26'4"N 77&deg;49'10"W</div>
        <div class="text address"><i>approx.</i> Walgreens, Martinsburg Pike, Shepherdstown, Jefferson County, West Virginia, 25443, United States</div>
        <div class="text dem precinct">Precinct Margin: D+33%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.248817286432164,-83.6413386080402&ll=42.248817286432164,-83.6413386080402&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/850.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.43472115,-77.8195830126343&ll=39.43472115,-77.8195830126343&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/851.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-853">Voter 853's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 853 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;31'17"N 98&deg;21'27"W</div>
        <div class="text address"><i>approx.</i> Crestway Road, Windcrest, Bexar County, Texas, 78239, United States</div>
        <div class="text dem precinct">Precinct Margin: D+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-854">Voter 854's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 854 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;11'50"N 93&deg;10'32"W</div>
        <div class="text address"><i>approx.</i> Lake Charles Loop, Greinwich Village, Lake Charles, Calcasieu Parish, Louisiana, 70602, United States</div>
        <div class="text dem precinct">Precinct Margin: D+88%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.521585130304505,-98.35764151562438&ll=29.521585130304505,-98.35764151562438&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/852.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.19741950442027,-93.1756595409517&ll=30.19741950442027,-93.1756595409517&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/853.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-855">Voter 855's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 855 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;51'41"N 77&deg;15'24"W</div>
        <div class="text address"><i>approx.</i> Chichester's Mill Race, Cross County Trail, Mantua, Fairfax County, Virginia, 22031, United States</div>
        <div class="text dem precinct">Precinct Margin: D+39%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-856">Voter 856's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 856 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;6'14"N 82&deg;4'6"W</div>
        <div class="text address"><i>approx.</i> 46, Juniper Loop Way, Marion County, Florida, 34480, United States</div>
        <div class="text gop precinct">Precinct Margin: R+22%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.86158421858925,-77.25684631520987&ll=38.86158421858925,-77.25684631520987&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/854.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.10397403959114,-82.0684062194333&ll=29.10397403959114,-82.0684062194333&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/855.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-857">Voter 857's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 857 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;23'11"N 74&deg;7'8"W</div>
        <div class="text address"><i>approx.</i> 15, Wychwood Road, Middletown, Middletown Township, Monmouth County, New Jersey, 07748, United States</div>
        <div class="text gop precinct">Precinct Margin: R+11%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-858">Voter 858's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 858 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;26'9"N 71&deg;5'3"W</div>
        <div class="text address"><i>approx.</i> 23, McCormack Street, Malden Highlands, Malden, Middlesex County, Massachusetts, 02176-6104, United States</div>
        <div class="text dem precinct">Precinct Margin: D+50%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.38658332335147,-74.11893084583743&ll=40.38658332335147,-74.11893084583743&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/856.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.4360194,-71.08431900639764&ll=42.4360194,-71.08431900639764&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/857.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-859">Voter 859's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 859 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;22'10"N 81&deg;20'0"W</div>
        <div class="text address"><i>approx.</i> 8658, Taylor May Road, Bainbridge Township, Geauga County, Ohio, 44023, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-860">Voter 860's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 860 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;6'34"N 75&deg;54'2"W</div>
        <div class="text address"><i>approx.</i> 74, Todd Road, Honey Brook Township, Chester County, Pennsylvania, 19344, United States</div>
        <div class="text gop precinct">Precinct Margin: R+30%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.36946494425758,-81.33338753604507&ll=41.36946494425758,-81.33338753604507&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/858.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.109643032497715,-75.90081932912159&ll=40.109643032497715,-75.90081932912159&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/859.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-861">Voter 861's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 861 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;24'54"N 79&deg;57'45"W</div>
        <div class="text address"><i>approx.</i> State Highway 45, Old Peru, Berkeley County, South Carolina, 29468, United States</div>
        <div class="text dem precinct">Precinct Margin: D+65%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-862">Voter 862's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 862 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;12'7"N 82&deg;3'28"W</div>
        <div class="text address"><i>approx.</i> Silver Springs Conservation Area, Northeast 51st Avenue, Marion County, Florida, 34470, United States</div>
        <div class="text gop precinct">Precinct Margin: R+40%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.4151011707535,-79.96264672515304&ll=33.4151011707535,-79.96264672515304&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/860.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.202009349999997,-82.0579598788451&ll=29.202009349999997,-82.0579598788451&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/861.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-863">Voter 863's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 863 voted for Biden</div> -->
        <div class="text coordinate"> 47&deg;57'15"N 117&deg;25'2"W</div>
        <div class="text address"><i>approx.</i> 307, West Deer Park-Milan Road, Spokane County, Washington, 99006, United States</div>
        <div class="text gop precinct">Precinct Margin: R+42%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-864">Voter 864's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 864 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;9'45"N 93&deg;9'28"W</div>
        <div class="text address"><i>approx.</i> 2572, 12th Street Northeast, College Heights, Mason City, Cerro Gordo County, Iowa, 50401, United States</div>
        <div class="text dem precinct">Precinct Margin: D+0%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=47.9543766,-117.41724651048&ll=47.9543766,-117.41724651048&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/862.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.16273344791907,-93.15799644831706&ll=43.16273344791907,-93.15799644831706&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/863.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-865">Voter 865's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 865 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;54'59"N 87&deg;42'0"W</div>
        <div class="text address"><i>approx.</i> 1927, North Richmond Street, Logan Square, Chicago, Cook County, Illinois, 60647, United States</div>
        <div class="text dem precinct">Precinct Margin: D+77%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-866">Voter 866's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 866 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;30'14"N 112&deg;1'25"W</div>
        <div class="text address"><i>approx.</i> Camelback High School, East Campbell Avenue, Camelback East, Phoenix, Maricopa County, Arizona, 85018-8830, United States</div>
        <div class="text dem precinct">Precinct Margin: D+14%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.916451949999995,-87.70019497368645&ll=41.916451949999995,-87.70019497368645&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/864.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.50408535,-112.02376287547183&ll=33.50408535,-112.02376287547183&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/865.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-867">Voter 867's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 867 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;59'23"N 72&deg;42'53"W</div>
        <div class="text address"><i>approx.</i> 196, Hobby Hill Road, Newfane, Windham County, Vermont, 05345, United States</div>
        <div class="text dem precinct">Precinct Margin: D+51%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-868">Voter 868's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 868 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;46'56"N 75&deg;53'16"W</div>
        <div class="text address"><i>approx.</i> 97, Hollow Rock Lane, New London, New London Township, Chester County, Pennsylvania, 19352, United States</div>
        <div class="text dem precinct">Precinct Margin: D+0%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.989893,-72.714829&ll=42.989893,-72.714829&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/866.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.78247886763205,-75.88784190117887&ll=39.78247886763205,-75.88784190117887&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/867.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-869">Voter 869's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 869 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;55'46"N 90&deg;19'42"W</div>
        <div class="text address"><i>approx.</i> 3198, Grand Avenue, Galesburg, Knox County, Illinois, 61401, United States</div>
        <div class="text dem precinct">Precinct Margin: D+10%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-870">Voter 870's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 870 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;24'33"N 75&deg;49'31"W</div>
        <div class="text address"><i>approx.</i> Upper Round Pond Road, Smithville Flats, Town of Smithville, Chenango County, New York, 13841, United States</div>
        <div class="text gop precinct">Precinct Margin: R+34%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.92963900676952,-90.32855871141439&ll=40.92963900676952,-90.32855871141439&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/868.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.40929391469011,-75.82535321842133&ll=42.40929391469011,-75.82535321842133&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/869.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-871">Voter 871's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 871 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;14'21"N 75&deg;33'30"W</div>
        <div class="text address"><i>approx.</i> Pottstown Heritage Field, 3310, West Ridge Pike, Limerick, Limerick Township, Montgomery County, Pennsylvania, 19464, United States</div>
        <div class="text dem precinct">Precinct Margin: D+11%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-872">Voter 872's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 872 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;9'58"N 80&deg;20'20"W</div>
        <div class="text address"><i>approx.</i> 4015, Talcott Avenue, Old Town, Winston-Salem, Forsyth County, North Carolina, 27106, United States</div>
        <div class="text dem precinct">Precinct Margin: D+17%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.2393406,-75.55847816317285&ll=40.2393406,-75.55847816317285&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/870.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.16630390476191,-80.33916566666666&ll=36.16630390476191,-80.33916566666666&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/871.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-873">Voter 873's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 873 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;42'18"N 75&deg;6'31"W</div>
        <div class="text address"><i>approx.</i> 52, Windsor Road, Rehoboth Beach Yacht & Country Club, Silver View Farm, Sussex County, Delaware, 19971, United States</div>
        <div class="text dem precinct">Precinct Margin: D+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-874">Voter 874's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 874 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;26'46"N 97&deg;49'42"W</div>
        <div class="text address"><i>approx.</i> 10801, Foundation Road, Austin, Travis County, Texas, 78726, United States</div>
        <div class="text dem precinct">Precinct Margin: D+36%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.7050071,-75.1087334&ll=38.7050071,-75.1087334&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/872.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.4462665,-97.8283622243029&ll=30.4462665,-97.8283622243029&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/873.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-875">Voter 875's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 875 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;42'53"N 122&deg;26'25"W</div>
        <div class="text address"><i>approx.</i> 10, Curtis Street, Crocker-Amazon, San Francisco, California, 94112, United States</div>
        <div class="text dem precinct">Precinct Margin: D+57%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-876">Voter 876's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 876 voted for Trump</div> -->
        <div class="text coordinate"> 29&deg;51'52"N 95&deg;35'57"W</div>
        <div class="text address"><i>approx.</i> 12798, Watercress Park, Villages at Lakepointe, Harris County, Texas, 77041, United States</div>
        <div class="text dem precinct">Precinct Margin: D+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.7149984,-122.44053282760737&ll=37.7149984,-122.44053282760737&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/874.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.864585,-95.599339&ll=29.864585,-95.599339&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/875.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-877">Voter 877's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 877 voted for Biden</div> -->
        <div class="text coordinate"> 26&deg;42'57"N 80&deg;9'34"W</div>
        <div class="text address"><i>approx.</i> Riverwalk Circle, Riverwalk, West Palm Beach, Palm Beach County, Florida, United States</div>
        <div class="text gop precinct">Precinct Margin: R+1%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-878">Voter 878's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 878 voted for Trump</div> -->
        <div class="text coordinate"> 46&deg;48'39"N 95&deg;50'13"W</div>
        <div class="text address"><i>approx.</i> 1298, Richard Avenue, Detroit Lakes, Becker County, Minnesota, 56501, United States</div>
        <div class="text gop precinct">Precinct Margin: R+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=26.7159175,-80.15958957607552&ll=26.7159175,-80.15958957607552&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/876.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=46.8111,-95.837208&ll=46.8111,-95.837208&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/877.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-879">Voter 879's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 879 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;44'35"N 105&deg;1'54"W</div>
        <div class="text address"><i>approx.</i> 1615, Julian Street, West Colfax, Denver, Colorado, 80204, United States</div>
        <div class="text dem precinct">Precinct Margin: D+64%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-880">Voter 880's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 880 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;13'25"N 80&deg;58'27"W</div>
        <div class="text address"><i>approx.</i> 198, Scarlet Oak, Meadows at New Riverside, Beaufort County, South Carolina, 29910, United States</div>
        <div class="text gop precinct">Precinct Margin: R+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.7432087,-105.03178569993113&ll=39.7432087,-105.03178569993113&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/878.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.223871,-80.9742097&ll=32.223871,-80.9742097&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/879.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-881">Voter 881's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 881 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;32'9"N 112&deg;3'59"W</div>
        <div class="text address"><i>approx.</i> 496, East Lamar Road, Phoenix, Maricopa County, Arizona, 85012, United States</div>
        <div class="text dem precinct">Precinct Margin: D+13%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-882">Voter 882's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 882 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;25'9"N 101&deg;18'34"W</div>
        <div class="text address"><i>approx.</i> Avenue 348 A, Hayes County, Nebraska, United States</div>
        <div class="text gop precinct">Precinct Margin: R+83%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.53598931755882,-112.06646934673986&ll=33.53598931755882,-112.06646934673986&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/880.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.419203327425635,-101.30971371103865&ll=40.419203327425635,-101.30971371103865&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/881.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-883">Voter 883's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 883 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;15'49"N 72&deg;10'16"W</div>
        <div class="text address"><i>approx.</i> 58, John Gilbert Road, West Brookfield, Worcester County, Massachusetts, 01585, United States</div>
        <div class="text dem precinct">Precinct Margin: D+0%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-884">Voter 884's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 884 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;32'30"N 87&deg;7'29"W</div>
        <div class="text address"><i>approx.</i> INDIANA OXIDE CORP, North Forest Avenue, Brazil, Brazil Township, Clay County, Indiana, 47834, United States</div>
        <div class="text gop precinct">Precinct Margin: R+54%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.26372035,-72.1711541760263&ll=42.26372035,-72.1711541760263&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/882.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.5418572,-87.1249057&ll=39.5418572,-87.1249057&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/883.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-885">Voter 885's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 885 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;42'34"N 111&deg;49'48"W</div>
        <div class="text address"><i>approx.</i> 2130, Atkin Avenue, Canyon Rim, Millcreek, Salt Lake County, Utah, 84109, United States</div>
        <div class="text dem precinct">Precinct Margin: D+36%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-886">Voter 886's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 886 voted for Trump</div> -->
        <div class="text coordinate"> 43&deg;4'58"N 94&deg;15'2"W</div>
        <div class="text address"><i>approx.</i> 220th Street, Algona, Kossuth County, Iowa, 50511, United States</div>
        <div class="text gop precinct">Precinct Margin: R+25%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.70952019259648,-111.8300753044365&ll=40.70952019259648,-111.8300753044365&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/884.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.08283780094319,-94.25075728625191&ll=43.08283780094319,-94.25075728625191&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/885.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-887">Voter 887's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 887 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;41'5"N 71&deg;31'12"W</div>
        <div class="text address"><i>approx.</i> 109, Church Street, Centerville, Crompton, West Warwick, Kent County, Rhode Island, 02893, United States</div>
        <div class="text dem precinct">Precinct Margin: D+6%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-888">Voter 888's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 888 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;22'34"N 75&deg;8'56"W</div>
        <div class="text address"><i>approx.</i> 11798, Manklin Creek Road, Ocean Pines, Worcester County, Maryland, 21811, United States</div>
        <div class="text gop precinct">Precinct Margin: R+12%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.68480054545455,-71.52019545454544&ll=41.68480054545455,-71.52019545454544&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/886.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.376118,-75.149108&ll=38.376118,-75.149108&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/887.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-889">Voter 889's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 889 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;37'32"N 117&deg;51'25"W</div>
        <div class="text address"><i>approx.</i> 1842, Port Margate Place, Newport Center, Newport Beach, Orange County, California, 92660, United States</div>
        <div class="text gop precinct">Precinct Margin: R+11%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-890">Voter 890's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 890 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;38'20"N 84&deg;13'13"W</div>
        <div class="text address"><i>approx.</i> Extended Stay America Dayton South, Lois Circle, Shanersville, Miami Township, Montgomery County, Ohio, 45459, United States</div>
        <div class="text gop precinct">Precinct Margin: R+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.6257587,-117.8571615&ll=33.6257587,-117.8571615&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/888.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.6389185,-84.22033957846449&ll=39.6389185,-84.22033957846449&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/889.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-891">Voter 891's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 891 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;41'3"N 86&deg;14'41"W</div>
        <div class="text address"><i>approx.</i> 620, North Niles Avenue, South Bend, Saint Joseph County, Indiana, 46617, United States</div>
        <div class="text dem precinct">Precinct Margin: D+52%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-892">Voter 892's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 892 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;28'35"N 90&deg;12'40"W</div>
        <div class="text address"><i>approx.</i> 320, Brellinger St, Brellinger Estates, Columbia, Monroe County, Illinois, 62236, United States</div>
        <div class="text gop precinct">Precinct Margin: R+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.684179438545804,-86.24476529597473&ll=41.684179438545804,-86.24476529597473&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/890.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.4764215,-90.2111437&ll=38.4764215,-90.2111437&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/891.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-893">Voter 893's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 893 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;16'0"N 75&deg;12'34"W</div>
        <div class="text address"><i>approx.</i> 121, Cambridge Place, New Britain Walk, New Britain Township, Bucks County, Pennsylvania, 18914, United States</div>
        <div class="text dem precinct">Precinct Margin: D+7%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-894">Voter 894's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 894 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;46'8"N 77&deg;13'14"W</div>
        <div class="text address"><i>approx.</i> School Track, Dog Walking Path, West Springfield, Fairfax County, Virginia, 22152, United States</div>
        <div class="text dem precinct">Precinct Margin: D+45%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.26681767274379,-75.20957687489401&ll=40.26681767274379,-75.20957687489401&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/892.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.7691577,-77.22070057941045&ll=38.7691577,-77.22070057941045&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/893.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-895">Voter 895's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 895 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;8'34"N 105&deg;6'32"W</div>
        <div class="text address"><i>approx.</i> 891, Quebec Avenue, Longmont, Boulder County, Colorado, 80501, United States</div>
        <div class="text dem precinct">Precinct Margin: D+37%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-896">Voter 896's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 896 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;41'27"N 72&deg;24'9"W</div>
        <div class="text address"><i>approx.</i> 343, East Street, Hebron, Tolland County, Connecticut, 06248, United States</div>
        <div class="text dem precinct">Precinct Margin: D+10%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.142788621272295,-105.10896009304955&ll=40.142788621272295,-105.10896009304955&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/894.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.6908724,-72.4026539&ll=41.6908724,-72.4026539&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/895.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-897">Voter 897's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 897 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;56'46"N 73&deg;57'42"W</div>
        <div class="text address"><i>approx.</i> 325, Piermont Road, Cresskill, Bergen County, New Jersey, 07626, United States</div>
        <div class="text dem precinct">Precinct Margin: D+16%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-898">Voter 898's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 898 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;33'27"N 112&deg;20'22"W</div>
        <div class="text address"><i>approx.</i> 7210, East Sienna Springs Lane, Prescott Valley, Yavapai County, Arizona, 86314, United States</div>
        <div class="text gop precinct">Precinct Margin: R+39%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.946352,-73.961683&ll=40.946352,-73.961683&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/896.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.55765833576843,-112.33971068446161&ll=34.55765833576843,-112.33971068446161&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/897.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-899">Voter 899's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 899 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;25'26"N 81&deg;27'28"W</div>
        <div class="text address"><i>approx.</i> 5012, Crofton Avenue, Solon, Cuyahoga County, Ohio, 44139, United States</div>
        <div class="text dem precinct">Precinct Margin: D+40%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-900">Voter 900's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 900 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;36'27"N 82&deg;25'55"W</div>
        <div class="text address"><i>approx.</i> Upper River Trail, Buncombe County, North Carolina, 28778, United States</div>
        <div class="text dem precinct">Precinct Margin: D+12%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.42402411764706,-81.45780347058823&ll=41.42402411764706,-81.45780347058823&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/898.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.6076835541504,-82.432179705822&ll=35.6076835541504,-82.432179705822&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/899.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-901">Voter 901's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 901 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;4'49"N 94&deg;32'48"W</div>
        <div class="text address"><i>approx.</i> True Vine Baptist Church, East 25th Street, Washington Wheatley, Kansas City, Jackson County, Missouri, 64127, United States</div>
        <div class="text dem precinct">Precinct Margin: D+89%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-902">Voter 902's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 902 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;55'44"N 104&deg;58'33"W</div>
        <div class="text address"><i>approx.</i> 12840, Clarkson Circle, Hunters Glen Lakeshore, Thornton, Adams County, Colorado, 80241, United States</div>
        <div class="text dem precinct">Precinct Margin: D+12%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.0803878,-94.54672780717456&ll=39.0803878,-94.54672780717456&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/900.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.92909465246775,-104.97594001402825&ll=39.92909465246775,-104.97594001402825&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/901.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-903">Voter 903's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 903 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;1'42"N 84&deg;12'50"W</div>
        <div class="text address"><i>approx.</i> 368, Dewpoint Lane, Johns Creek, Fulton County, Georgia, 30022, United States</div>
        <div class="text gop precinct">Precinct Margin: R+2%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-904">Voter 904's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 904 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;28'35"N 85&deg;7'23"W</div>
        <div class="text address"><i>approx.</i> 16795, Northwest Magnolia Church Road, Calhoun County, Florida, 32421, United States</div>
        <div class="text gop precinct">Precinct Margin: R+65%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.0283508332523,-84.21411530802433&ll=34.0283508332523,-84.21411530802433&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/902.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.476615355116564,-85.12320497863678&ll=30.476615355116564,-85.12320497863678&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/903.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-905">Voter 905's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 905 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;0'36"N 81&deg;32'12"W</div>
        <div class="text address"><i>approx.</i> Leo Maguire Road, Elwood, Saint Johns County, Florida, 32092, United States</div>
        <div class="text gop precinct">Precinct Margin: R+32%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-906">Voter 906's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 906 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;47'9"N 75&deg;37'13"W</div>
        <div class="text address"><i>approx.</i> 62, Westbrae Lane, New Castle County, Delaware, 19807, United States</div>
        <div class="text dem precinct">Precinct Margin: D+21%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.01010311730851,-81.53675733167026&ll=30.01010311730851,-81.53675733167026&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/904.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.78593247980709,-75.62030523010851&ll=39.78593247980709,-75.62030523010851&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/905.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-907">Voter 907's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 907 voted for Biden</div> -->
        <div class="text coordinate"> 30&deg;26'26"N 87&deg;12'3"W</div>
        <div class="text address"><i>approx.</i> 1565, East Scott Street, Pensacola, Escambia County, Florida, 32503, United States</div>
        <div class="text dem precinct">Precinct Margin: D+15%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-908">Voter 908's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 908 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;3'2"N 94&deg;30'4"W</div>
        <div class="text address"><i>approx.</i> 1398, East 34th Street, Dennis Acres, Joplin, Newton County, Missouri, 64804, United States</div>
        <div class="text gop precinct">Precinct Margin: R+37%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.44073187755102,-87.2009166734694&ll=30.44073187755102,-87.2009166734694&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/906.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.050756,-94.501314&ll=37.050756,-94.501314&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/907.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-909">Voter 909's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 909 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;11'57"N 118&deg;24'42"W</div>
        <div class="text address"><i>approx.</i> 7095, Goodland Avenue, Valley Glen, Los Angeles, Los Angeles County, California, 91605, United States</div>
        <div class="text dem precinct">Precinct Margin: D+41%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-910">Voter 910's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 910 voted for Trump</div> -->
        <div class="text coordinate"> 41&deg;11'18"N 111&deg;55'55"W</div>
        <div class="text address"><i>approx.</i> 1808, Acorn Circle, Ogden, Weber County, Utah, 84403, United States</div>
        <div class="text dem precinct">Precinct Margin: D+3%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.19918690909091,-118.411705&ll=34.19918690909091,-118.411705&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/908.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.188557591372174,-111.93209685575843&ll=41.188557591372174,-111.93209685575843&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/909.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-911">Voter 911's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 911 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;11'37"N 115&deg;14'23"W</div>
        <div class="text address"><i>approx.</i> 1999, Joliet Circle, Inner Northwest, Las Vegas, Clark County, Nevada, 89108, United States</div>
        <div class="text dem precinct">Precinct Margin: D+33%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-912">Voter 912's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 912 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;9'47"N 94&deg;40'52"W</div>
        <div class="text address"><i>approx.</i> Southeast Wyandotte Road, Cherokee County, Kansas, 66728, United States</div>
        <div class="text gop precinct">Precinct Margin: R+63%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.193652,-115.239821&ll=36.193652,-115.239821&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/910.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.1631968712243,-94.6811963520482&ll=37.1631968712243,-94.6811963520482&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/911.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-913">Voter 913's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 913 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;33'51"N 121&deg;52'56"W</div>
        <div class="text address"><i>approx.</i> 3735, Andrade Road, Alameda County, California, 94586, United States</div>
        <div class="text dem precinct">Precinct Margin: D+23%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-914">Voter 914's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 914 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;26'49"N 83&deg;48'51"W</div>
        <div class="text address"><i>approx.</i> 5481, Northwoods Road, Hall County, Georgia, 30527, United States</div>
        <div class="text gop precinct">Precinct Margin: R+75%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.56425013648957,-121.88232170618394&ll=37.56425013648957,-121.88232170618394&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/912.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.44703876268609,-83.81422265127732&ll=34.44703876268609,-83.81422265127732&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/913.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-915">Voter 915's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 915 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;32'1"N 90&deg;50'13"W</div>
        <div class="text address"><i>approx.</i> 17897, Brehm Lane, Dubuque County, Iowa, 52002, United States</div>
        <div class="text gop precinct">Precinct Margin: R+14%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-916">Voter 916's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 916 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;41'26"N 73&deg;42'59"W</div>
        <div class="text address"><i>approx.</i> 8, Frick Street, Alden Manor, Town of Hempstead, Nassau County, New York, 11003, United States</div>
        <div class="text dem precinct">Precinct Margin: D+80%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.5338087,-90.8370614&ll=42.5338087,-90.8370614&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/914.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.6906646,-73.7165601&ll=40.6906646,-73.7165601&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/915.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-917">Voter 917's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 917 voted for Biden</div> -->
        <div class="text coordinate"> 31&deg;45'58"N 89&deg;10'24"W</div>
        <div class="text address"><i>approx.</i> 3, Greenbriar Lane, Jones County, Mississippi, 39440, United States</div>
        <div class="text gop precinct">Precinct Margin: R+40%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-918">Voter 918's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 918 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;44'5"N 81&deg;25'25"W</div>
        <div class="text address"><i>approx.</i> 9094, Arary Est Road, Hildebran, Burke County, North Carolina, 28612, United States</div>
        <div class="text gop precinct">Precinct Margin: R+58%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=31.7663545152529,-89.17349424054883&ll=31.7663545152529,-89.17349424054883&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/916.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.73488078820875,-81.42387139152977&ll=35.73488078820875,-81.42387139152977&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/917.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-919">Voter 919's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 919 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;36'10"N 82&deg;51'48"W</div>
        <div class="text address"><i>approx.</i> Elmo Street, Clemens Park, Charter Township of Clinton, Clinton Township, Macomb County, Michigan, 48043-5640, United States</div>
        <div class="text dem precinct">Precinct Margin: D+0%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-920">Voter 920's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 920 voted for Trump</div> -->
        <div class="text coordinate"> 42&deg;2'8"N 91&deg;42'26"W</div>
        <div class="text address"><i>approx.</i> Saint Lukes Northridge Family Medicine, South Blairsferry Crossing, Heritage Creek, Cedar Rapids, Hiawatha, Linn County, Iowa, 52233, United States</div>
        <div class="text dem precinct">Precinct Margin: D+8%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.60278658075938,-82.86349636417185&ll=42.60278658075938,-82.86349636417185&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/918.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.0356931,-91.7072454&ll=42.0356931,-91.7072454&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/919.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-921">Voter 921's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 921 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;43'19"N 84&deg;11'9"W</div>
        <div class="text address"><i>approx.</i> 2399, Creekview Trail, DeKalb County, Georgia, 30035, United States</div>
        <div class="text dem precinct">Precinct Margin: D+92%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-922">Voter 922's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 922 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;3'52"N 104&deg;48'36"W</div>
        <div class="text address"><i>approx.</i> Forest Chime Place, El Paso County, Colorado, 80132-7725, United States</div>
        <div class="text gop precinct">Precinct Margin: R+32%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.721991,-84.185976&ll=33.721991,-84.185976&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/920.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.064459053147814,-104.81007632564311&ll=39.064459053147814,-104.81007632564311&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/921.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-923">Voter 923's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 923 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;52'8"N 84&deg;46'36"W</div>
        <div class="text address"><i>approx.</i> 8494, Quackenbush Road, Reading Township, Hillsdale County, Michigan, 49274, United States</div>
        <div class="text gop precinct">Precinct Margin: R+39%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-924">Voter 924's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 924 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;46'6"N 76&deg;19'10"W</div>
        <div class="text address"><i>approx.</i> 1090, Beaver Dam Road, Butler Township, Schuylkill County, Pennsylvania, 17921, United States</div>
        <div class="text gop precinct">Precinct Margin: R+44%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.8688927980481,-84.77692821697325&ll=41.8688927980481,-84.77692821697325&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/922.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.7685376,-76.3196386&ll=40.7685376,-76.3196386&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/923.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-925">Voter 925's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 925 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;49'23"N 95&deg;11'45"W</div>
        <div class="text address"><i>approx.</i> Crosby Freeway Frontage Road, Beaumont Place, Harris County, Texas, 77049, United States</div>
        <div class="text dem precinct">Precinct Margin: D+53%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-926">Voter 926's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 926 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;28'6"N 86&deg;17'37"W</div>
        <div class="text address"><i>approx.</i> 5237, Coosada Ferry Road, Montgomery, Montgomery County, Alabama, 36110, United States</div>
        <div class="text gop precinct">Precinct Margin: R+13%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.823078487540823,-95.19606218256375&ll=29.823078487540823,-95.19606218256375&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/924.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.46860608407336,-86.29388843244777&ll=32.46860608407336,-86.29388843244777&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/925.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-927">Voter 927's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 927 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;15'35"N 85&deg;37'59"W</div>
        <div class="text address"><i>approx.</i> 3565, Parkview Avenue, Kalamazoo, Oshtemo Township, Kalamazoo County, Michigan, 49008, United States</div>
        <div class="text dem precinct">Precinct Margin: D+44%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-928">Voter 928's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 928 voted for Trump</div> -->
        <div class="text coordinate"> 39&deg;29'15"N 77&deg;56'2"W</div>
        <div class="text address"><i>approx.</i> 98, Trafalgar Circle, Princeton Shoals, Martinsburg, Berkeley County, West Virginia, 25404, United States</div>
        <div class="text gop precinct">Precinct Margin: R+31%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.259924653061226,-85.63330440816326&ll=42.259924653061226,-85.63330440816326&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/926.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.487618,-77.934122&ll=39.487618,-77.934122&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/927.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-929">Voter 929's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 929 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;48'51"N 104&deg;46'23"W</div>
        <div class="text address"><i>approx.</i> 1180, Server Drive, Colorado Springs, El Paso County, Colorado, 80910, United States</div>
        <div class="text dem precinct">Precinct Margin: D+13%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-930">Voter 930's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 930 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;29'24"N 90&deg;26'42"W</div>
        <div class="text address"><i>approx.</i> 17090, Parker Lane, Hammond, Tangipahoa Parish, Louisiana, 70403, United States</div>
        <div class="text gop precinct">Precinct Margin: R+43%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.81418839884551,-104.77328545852552&ll=38.81418839884551,-104.77328545852552&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/928.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.49003002585996,-90.44507240238671&ll=30.49003002585996,-90.44507240238671&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/929.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-931">Voter 931's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 931 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;18'34"N 121&deg;46'45"W</div>
        <div class="text address"><i>approx.</i> Evergreen Elementary School, 3010, Fowler Road, Evergreen, San Jose, Santa Clara County, California, 95135, United States</div>
        <div class="text dem precinct">Precinct Margin: D+45%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-932">Voter 932's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 932 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;4'58"N 117&deg;3'46"W</div>
        <div class="text address"><i>approx.</i> 410, Park Ranch Place, Escondido, San Diego County, California, 92025, United States</div>
        <div class="text dem precinct">Precinct Margin: D+2%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.309618400000005,-121.77934049449917&ll=37.309618400000005,-121.77934049449917&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/930.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.082874,-117.06288&ll=33.082874,-117.06288&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/931.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-933">Voter 933's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 933 voted for Biden</div> -->
        <div class="text coordinate"> 43&deg;20'29"N 72&deg;7'5"W</div>
        <div class="text address"><i>approx.</i> 101, Nutting Road, Sunapee, Sullivan County, New Hampshire, 03782, United States</div>
        <div class="text dem precinct">Precinct Margin: D+9%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-934">Voter 934's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 934 voted for Trump</div> -->
        <div class="text coordinate"> 32&deg;54'40"N 117&deg;5'20"W</div>
        <div class="text address"><i>approx.</i> 10455, Mountain Glen Terrace, San Diego, San Diego County, California, 92131, United States</div>
        <div class="text dem precinct">Precinct Margin: D+25%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=43.341408,-72.118224&ll=43.341408,-72.118224&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/932.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.911148,-117.089017&ll=32.911148,-117.089017&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/933.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-935">Voter 935's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 935 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;4'36"N 93&deg;12'24"W</div>
        <div class="text address"><i>approx.</i> 844, Minnesota Avenue, Owatonna, Steele County, Minnesota, 55060, United States</div>
        <div class="text gop precinct">Precinct Margin: R+19%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-936">Voter 936's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 936 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;27'18"N 91&deg;1'39"W</div>
        <div class="text address"><i>approx.</i> 14019, Katherine Avenue, Sherwood Garden Homes, Baton Rouge, East Baton Rouge Parish, Louisiana, 70815, United States</div>
        <div class="text dem precinct">Precinct Margin: D+29%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.076736775369625,-93.20685625091109&ll=44.076736775369625,-93.20685625091109&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/934.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.455112063333953,-91.02769709383605&ll=30.455112063333953,-91.02769709383605&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/935.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-937">Voter 937's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 937 voted for Biden</div> -->
        <div class="text coordinate"> 38&deg;34'48"N 121&deg;27'29"W</div>
        <div class="text address"><i>approx.</i> Blues Alley, Sacramento, Sacramento County, California, 95816, United States</div>
        <div class="text dem precinct">Precinct Margin: D+59%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-938">Voter 938's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 938 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;0'14"N 79&deg;48'6"W</div>
        <div class="text address"><i>approx.</i> 547, Dockery Road, Richmond County, North Carolina, 28379, United States</div>
        <div class="text gop precinct">Precinct Margin: R+30%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.58006450248661,-121.45824999606874&ll=38.58006450248661,-121.45824999606874&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/936.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.004011,-79.8017&ll=35.004011,-79.8017&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/937.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-939">Voter 939's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 939 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;20'15"N 79&deg;50'58"W</div>
        <div class="text address"><i>approx.</i> 2316, Riverview Avenue, Eden Park, McKeesport, Allegheny County, Pennsylvania, 15132, United States</div>
        <div class="text dem precinct">Precinct Margin: D+38%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-940">Voter 940's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 940 voted for Trump</div> -->
        <div class="text coordinate"> 35&deg;59'52"N 115&deg;17'15"W</div>
        <div class="text address"><i>approx.</i> West Cactus Avenue, Clark County, Nevada, 89178, United States</div>
        <div class="text dem precinct">Precinct Margin: D+13%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.337712606487266,-79.849568646932&ll=40.337712606487266,-79.849568646932&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/938.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.99790058479712,-115.28767596858282&ll=35.99790058479712,-115.28767596858282&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/939.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-941">Voter 941's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 941 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;23'21"N 96&deg;36'58"W</div>
        <div class="text address"><i>approx.</i> 2501, Hitching Post Trail, Anna, Collin County, Texas, 75409, United States</div>
        <div class="text gop precinct">Precinct Margin: R+24%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-942">Voter 942's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 942 voted for Trump</div> -->
        <div class="text coordinate"> 38&deg;35'28"N 121&deg;18'0"W</div>
        <div class="text address"><i>approx.</i> 10486, Investment Circle, Rancho Cordova, Sacramento County, California, 95670, United States</div>
        <div class="text dem precinct">Precinct Margin: D+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.389228125841235,-96.61632505034085&ll=33.389228125841235,-96.61632505034085&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/940.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=38.59120064250005,-121.30010279301771&ll=38.59120064250005,-121.30010279301771&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/941.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-943">Voter 943's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 943 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;2'56"N 121&deg;53'19"W</div>
        <div class="text address"><i>approx.</i> Aptos Creek Fire Road, Santa Cruz County, California, United States</div>
        <div class="text dem precinct">Precinct Margin: D+51%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-944">Voter 944's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 944 voted for Trump</div> -->
        <div class="text coordinate"> 34&deg;1'57"N 117&deg;45'43"W</div>
        <div class="text address"><i>approx.</i> 93, Quiet Hollow Road, Pomona, Los Angeles County, California, 91766, United States</div>
        <div class="text dem precinct">Precinct Margin: D+28%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.04891,-121.888643&ll=37.04891,-121.888643&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/942.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.03271859183673,-117.76215440816327&ll=34.03271859183673,-117.76215440816327&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/943.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-945">Voter 945's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 945 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;57'58"N 93&deg;17'12"W</div>
        <div class="text address"><i>approx.</i> Parsonage, Groveland Avenue, Loring Park, Minneapolis, Hennepin County, Minnesota, 554, United States</div>
        <div class="text dem precinct">Precinct Margin: D+79%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-946">Voter 946's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 946 voted for Trump</div> -->
        <div class="text coordinate"> 30&deg;26'58"N 91&deg;5'16"W</div>
        <div class="text address"><i>approx.</i> 9114, Woodbine Street, Broadmoor, Castlewood, Baton Rouge, East Baton Rouge Parish, Louisiana, 70815, United States</div>
        <div class="text gop precinct">Precinct Margin: R+11%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.966342749999995,-93.28693451546573&ll=44.966342749999995,-93.28693451546573&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/944.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=30.449672721366145,-91.08790719292777&ll=30.449672721366145,-91.08790719292777&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/945.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-947">Voter 947's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 947 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;15'37"N 80&deg;50'52"W</div>
        <div class="text address"><i>approx.</i> McGuffey PK-8, Tod Avenue Northwest, Hardscrabble, Warren, Trumbull County, Ohio, 44485, United States</div>
        <div class="text dem precinct">Precinct Margin: D+34%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-948">Voter 948's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 948 voted for Trump</div> -->
        <div class="text coordinate"> 37&deg;58'55"N 122&deg;32'42"W</div>
        <div class="text address"><i>approx.</i> 143, Solano Street, San Rafael, Marin County, California, 94901, United States</div>
        <div class="text dem precinct">Precinct Margin: D+72%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.26041695,-80.84785277510633&ll=41.26041695,-80.84785277510633&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/946.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.98217279106473,-122.54512411260961&ll=37.98217279106473,-122.54512411260961&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/947.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-949">Voter 949's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 949 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;2'1"N 118&deg;21'55"W</div>
        <div class="text address"><i>approx.</i> 2504, South Curson Avenue, West Adams, Los Angeles, Los Angeles County, California, 90016, United States</div>
        <div class="text dem precinct">Precinct Margin: D+79%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-950">Voter 950's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 950 voted for Trump</div> -->
        <div class="text coordinate"> 33&deg;34'37"N 82&deg;11'53"W</div>
        <div class="text address"><i>approx.</i> Columbia Land Corporation Lake, Cumberland Court, Lake Cumberland West, Columbia County, Georgia, 30809, United States</div>
        <div class="text gop precinct">Precinct Margin: R+40%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.03371741035639,-118.36539536189399&ll=34.03371741035639,-118.36539536189399&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/948.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.5770884,-82.1983055&ll=33.5770884,-82.1983055&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/949.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-951">Voter 951's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 951 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;37'43"N 86&deg;16'14"W</div>
        <div class="text address"><i>approx.</i> 440, West Ireland Road, South Bend, Saint Joseph County, Indiana, 46614, United States</div>
        <div class="text gop precinct">Precinct Margin: R+28%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-952">Voter 952's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 952 voted for Trump</div> -->
        <div class="text coordinate"> 40&deg;21'40"N 74&deg;16'19"W</div>
        <div class="text address"><i>approx.</i> 222, Tennent Road, Marlboro Summit, Marlboro Township, Monmouth County, New Jersey, 07751, United States</div>
        <div class="text dem precinct">Precinct Margin: D+1%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.62864720224205,-86.27059011278874&ll=41.62864720224205,-86.27059011278874&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/950.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.36133360962114,-74.27204682981235&ll=40.36133360962114,-74.27204682981235&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/951.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-953">Voter 953's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 953 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;49'2"N 75&deg;2'18"W</div>
        <div class="text address"><i>approx.</i> 50, Kelly Driver Road, Village of Glen Oaks, Gloucester Township, Camden County, New Jersey, 08021, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    

    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-954">Voter 954's Neighborhood</h3>

        <!-- <div class="text gop voter">Voter 954 voted for Trump</div> -->
        <div class="text coordinate"> 36&deg;25'59"N 81&deg;26'6"W</div>
        <div class="text address"><i>approx.</i> 623, Ashe Central School Road, Ashe County, North Carolina, 28640, United States</div>
        <div class="text gop precinct">Precinct Margin: R+44%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.81732075454228,-75.03846935561076&ll=39.81732075454228,-75.03846935561076&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/952.png"/>
            </a>
        </div>
    </td>
    

    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.433073276506875,-81.43526443444235&ll=36.433073276506875,-81.43526443444235&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/953.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-955">Voter 955's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 955 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;32'35"N 98&deg;19'5"W</div>
        <div class="text address"><i>approx.</i> Kitty Hawk Middle School, 840, Old Cimarron Trail, Universal City, Bexar County, Texas, 78148, United States</div>
        <div class="text dem precinct">Precinct Margin: D+14%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.543211149999998,-98.31816933363703&ll=29.543211149999998,-98.31816933363703&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/954.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-956">Voter 956's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 956 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;59'51"N 84&deg;34'8"W</div>
        <div class="text address"><i>approx.</i> Noonday Church, Noonday Church Road, Marietta, Cobb County, Georgia, 30066, United States</div>
        <div class="text dem precinct">Precinct Margin: D+40%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.997763,-84.569028&ll=33.997763,-84.569028&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/955.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-957">Voter 957's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 957 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;15'32"N 117&deg;12'55"W</div>
        <div class="text address"><i>approx.</i> 26998, Golf Course Lane, Lake Arrowhead, San Bernardino County, California, 92352, United States</div>
        <div class="text gop precinct">Precinct Margin: R+18%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.258934,-117.215545&ll=34.258934,-117.215545&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/956.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-958">Voter 958's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 958 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;54'51"N 118&deg;19'13"W</div>
        <div class="text address"><i>approx.</i> 13001, Purche Avenue, Gardena, Los Angeles County, California, 90249, United States</div>
        <div class="text dem precinct">Precinct Margin: D+81%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.91442424398749,-118.32050923985788&ll=33.91442424398749,-118.32050923985788&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/957.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-959">Voter 959's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 959 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;2'21"N 115&deg;14'22"W</div>
        <div class="text address"><i>approx.</i> 6778, Cavatina Avenue, Enterprise, Clark County, Nevada, 89139, United States</div>
        <div class="text dem precinct">Precinct Margin: D+21%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.0394094489796,-115.23969989795918&ll=36.0394094489796,-115.23969989795918&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/958.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-960">Voter 960's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 960 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;39'3"N 83&deg;37'17"W</div>
        <div class="text address"><i>approx.</i> 3341, Inverness Avenue, Secor Gardens, Vulcan, Toledo, Lucas County, Ohio, 43607, United States</div>
        <div class="text dem precinct">Precinct Margin: D+88%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.6509397755102,-83.62162146938776&ll=41.6509397755102,-83.62162146938776&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/959.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-961">Voter 961's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 961 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;4'30"N 123&deg;5'48"W</div>
        <div class="text address"><i>approx.</i> 1199, Lorella Avenue, Eugene, Lane County, Oregon, 97401, United States</div>
        <div class="text dem precinct">Precinct Margin: D+44%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.075068,-123.09691&ll=44.075068,-123.09691&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/960.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-962">Voter 962's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 962 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;59'39"N 85&deg;55'25"W</div>
        <div class="text address"><i>approx.</i> 5349, Warner Street, Allendale, Allendale Charter Township, Ottawa County, Michigan, 49401, United States</div>
        <div class="text gop precinct">Precinct Margin: R+15%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.99423445336958,-85.92367928060959&ll=42.99423445336958,-85.92367928060959&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/961.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-963">Voter 963's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 963 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;38'49"N 74&deg;23'21"W</div>
        <div class="text address"><i>approx.</i> Forest Road Building, Forest Road, Fanwood, Union County, New Jersey, 07023, United States</div>
        <div class="text dem precinct">Precinct Margin: D+36%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.64716695,-74.38926083307462&ll=40.64716695,-74.38926083307462&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/962.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-964">Voter 964's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 964 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;55'1"N 88&deg;10'39"W</div>
        <div class="text address"><i>approx.</i> Stonecrest Road, Glen Oaks Estates, Muskego, Waukesha County, Wisconsin, 53150, United States</div>
        <div class="text gop precinct">Precinct Margin: R+41%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.91703643055487,-88.17750209474282&ll=42.91703643055487,-88.17750209474282&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/963.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-965">Voter 965's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 965 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;16'4"N 76&deg;51'10"W</div>
        <div class="text address"><i>approx.</i> 3799, Garand Road, Gray Rock, Howard County, Maryland, 21042, United States</div>
        <div class="text dem precinct">Precinct Margin: D+37%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.267831,-76.85287&ll=39.267831,-76.85287&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/964.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-966">Voter 966's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 966 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;45'46"N 111&deg;55'59"W</div>
        <div class="text address"><i>approx.</i> North Oak Creek Valley Road, Yavapai County, Arizona, 86325, United States</div>
        <div class="text gop precinct">Precinct Margin: R+16%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.76303359091654,-111.93310137099073&ll=34.76303359091654,-111.93310137099073&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/965.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-967">Voter 967's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 967 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;7'39"N 85&deg;56'40"W</div>
        <div class="text address"><i>approx.</i> 40910, 80th Avenue, Decatur Township, Van Buren County, Michigan, 49045, United States</div>
        <div class="text gop precinct">Precinct Margin: R+16%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.127693689041934,-85.94457385373559&ll=42.127693689041934,-85.94457385373559&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/966.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-968">Voter 968's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 968 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;46'22"N 97&deg;20'2"W</div>
        <div class="text address"><i>approx.</i> 1301, Poindexter Avenue, Fort Worth, Tarrant County, Texas, 76196, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.77303095,-97.33410153907593&ll=32.77303095,-97.33410153907593&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/967.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-969">Voter 969's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 969 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;16'57"N 88&deg;6'12"W</div>
        <div class="text address"><i>approx.</i> Greenleaf Road, Askeaton, Town of Holland, Brown County, Wisconsin, 54123, United States</div>
        <div class="text gop precinct">Precinct Margin: R+40%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.28257610084377,-88.10338314600278&ll=44.28257610084377,-88.10338314600278&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/968.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-970">Voter 970's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 970 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;4'50"N 75&deg;10'43"W</div>
        <div class="text address"><i>approx.</i> 8639, Temple Road, Ivy Hill, Philadelphia, Philadelphia County, Pennsylvania, 19150, United States</div>
        <div class="text dem precinct">Precinct Margin: D+93%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.080825571428576,-75.17865871428572&ll=40.080825571428576,-75.17865871428572&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/969.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-971">Voter 971's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 971 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;11'5"N 82&deg;53'40"W</div>
        <div class="text address"><i>approx.</i> 8158, Oxbow Road, Genoa Township, Delaware County, Ohio, 43082, United States</div>
        <div class="text gop precinct">Precinct Margin: R+24%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.18484800648341,-82.89462477113607&ll=40.18484800648341,-82.89462477113607&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/970.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-972">Voter 972's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 972 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;35'12"N 98&deg;29'31"W</div>
        <div class="text address"><i>approx.</i> 241, Limestone Creek, Hill Country Village, Bexar County, Texas, 78232, United States</div>
        <div class="text gop precinct">Precinct Margin: R+34%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.58682398795251,-98.49212856889723&ll=29.58682398795251,-98.49212856889723&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/971.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-973">Voter 973's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 973 voted for Biden</div> -->
        <div class="text coordinate"> 34&deg;6'13"N 117&deg;41'22"W</div>
        <div class="text address"><i>approx.</i> North Central Avenue, College Heights, Upland, San Bernardino County, California, 91786, United States</div>
        <div class="text dem precinct">Precinct Margin: D+42%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=34.1038550406956,-117.68967907187682&ll=34.1038550406956,-117.68967907187682&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/972.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-974">Voter 974's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 974 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;16'22"N 74&deg;31'39"W</div>
        <div class="text address"><i>approx.</i> 189, Park Avenue, Hightstown, Mercer County, New Jersey, 08520, United States</div>
        <div class="text dem precinct">Precinct Margin: D+39%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.272896897959185,-74.52760728571428&ll=40.272896897959185,-74.52760728571428&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/973.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-975">Voter 975's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 975 voted for Biden</div> -->
        <div class="text coordinate"> 41&deg;43'26"N 71&deg;18'48"W</div>
        <div class="text address"><i>approx.</i> 98, Bluff Road, Barrington, Bristol County, Rhode Island, 02806, United States</div>
        <div class="text dem precinct">Precinct Margin: D+47%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=41.72405028304375,-71.31358567008259&ll=41.72405028304375,-71.31358567008259&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/974.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-976">Voter 976's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 976 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;19'10"N 89&deg;32'22"W</div>
        <div class="text address"><i>approx.</i> 8207, North 22nd Avenue, Raymond, Montgomery County, Illinois, 62560, United States</div>
        <div class="text gop precinct">Precinct Margin: R+50%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.31969470077654,-89.53957454909397&ll=39.31969470077654,-89.53957454909397&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/975.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-977">Voter 977's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 977 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;6'21"N 93&deg;22'6"W</div>
        <div class="text address"><i>approx.</i> 6785, 83rd Place North, Brooklyn Park, Hennepin County, Minnesota, 55445, United States</div>
        <div class="text dem precinct">Precinct Margin: D+48%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.10597757142857,-93.36837657142857&ll=45.10597757142857,-93.36837657142857&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/976.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-978">Voter 978's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 978 voted for Biden</div> -->
        <div class="text coordinate"> 32&deg;21'48"N 86&deg;17'36"W</div>
        <div class="text address"><i>approx.</i> Alabama State University, 915, South Jackson Street, Montgomery, Montgomery County, Alabama, 36104, United States</div>
        <div class="text dem precinct">Precinct Margin: D+79%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=32.36334985,-86.29355107996977&ll=32.36334985,-86.29355107996977&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/977.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-979">Voter 979's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 979 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;7'14"N 72&deg;44'33"W</div>
        <div class="text address"><i>approx.</i> 32, George Street, Westfield, Hampden County, Massachusetts, 01085-3899, United States</div>
        <div class="text dem precinct">Precinct Margin: D+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.12071125,-72.74276884410772&ll=42.12071125,-72.74276884410772&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/978.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-980">Voter 980's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 980 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;33'15"N 119&deg;57'47"W</div>
        <div class="text address"><i>approx.</i> Hawk Meadow Trail, Verdi-Mogul, Washoe County, Nevada, 89523-3829, United States</div>
        <div class="text gop precinct">Precinct Margin: R+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.5542068,-119.9631393&ll=39.5542068,-119.9631393&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/979.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-981">Voter 981's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 981 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;52'57"N 78&deg;44'42"W</div>
        <div class="text address"><i>approx.</i> 3168, Glen Royal Road, Raleigh, Wake County, North Carolina, 27617, United States</div>
        <div class="text dem precinct">Precinct Margin: D+40%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.882511250258155,-78.74511922043715&ll=35.882511250258155,-78.74511922043715&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/980.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-982">Voter 982's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 982 voted for Biden</div> -->
        <div class="text coordinate"> 28&deg;41'24"N 81&deg;22'18"W</div>
        <div class="text address"><i>approx.</i> 1600, Orlando Avenue, Seminole County, Florida, 32750, United States</div>
        <div class="text gop precinct">Precinct Margin: R+6%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=28.690094,-81.371824&ll=28.690094,-81.371824&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/981.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-983">Voter 983's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 983 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;43'55"N 73&deg;51'41"W</div>
        <div class="text address"><i>approx.</i> 97th Street & 62nd Drive, 97th Street, Rego Park, Queens, City of New York, New York, 11374, United States</div>
        <div class="text dem precinct">Precinct Margin: D+3%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.7321154,-73.8616238&ll=40.7321154,-73.8616238&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/982.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-984">Voter 984's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 984 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;16'30"N 70&deg;30'11"W</div>
        <div class="text address"><i>approx.</i> 32, Richards Lane, Paris, Oxford County, Maine, 04281, United States</div>
        <div class="text gop precinct">Precinct Margin: R+7%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.27503163633683,-70.50308822924735&ll=44.27503163633683,-70.50308822924735&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/983.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-985">Voter 985's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 985 voted for Biden</div> -->
        <div class="text coordinate"> 45&deg;1'56"N 87&deg;11'49"W</div>
        <div class="text address"><i>approx.</i> 3435, Fairview Road, Town of Baileys Harbor, Door County, Wisconsin, 54202, United States</div>
        <div class="text dem precinct">Precinct Margin: D+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=45.03235112016315,-87.19694460661005&ll=45.03235112016315,-87.19694460661005&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/984.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-986">Voter 986's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 986 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;9'42"N 76&deg;30'57"W</div>
        <div class="text address"><i>approx.</i> 448, Waverly Place, Windsor Great Park, Newport News, Virginia, 23608, United States</div>
        <div class="text dem precinct">Precinct Margin: D+21%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.161834400000004,-76.51603260611313&ll=37.161834400000004,-76.51603260611313&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/985.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-987">Voter 987's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 987 voted for Biden</div> -->
        <div class="text coordinate"> 44&deg;2'58"N 123&deg;0'25"W</div>
        <div class="text address"><i>approx.</i> 1299, D Street, Springfield, Lane County, Oregon, 97477, United States</div>
        <div class="text dem precinct">Precinct Margin: D+20%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=44.04956404717363,-123.00710123410067&ll=44.04956404717363,-123.00710123410067&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/986.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-988">Voter 988's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 988 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;19'41"N 119&deg;3'42"W</div>
        <div class="text address"><i>approx.</i> 5448, Cherry Tree Lane, Bakersfield, Kern County, California, 93309, United States</div>
        <div class="text dem precinct">Precinct Margin: D+19%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.328237751429164,-119.0617198029436&ll=35.328237751429164,-119.0617198029436&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/987.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-989">Voter 989's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 989 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;38'29"N 112&deg;4'45"W</div>
        <div class="text address"><i>approx.</i> WinCo Foods, 330, West Bell Road, Central Park Village, Phoenix, Maricopa County, Arizona, 85023, United States</div>
        <div class="text dem precinct">Precinct Margin: D+0%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.64152275,-112.0793539208352&ll=33.64152275,-112.0793539208352&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/988.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-990">Voter 990's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 990 voted for Biden</div> -->
        <div class="text coordinate"> 39&deg;30'39"N 75&deg;40'47"W</div>
        <div class="text address"><i>approx.</i> Jamison Corner Road, New Castle County, Delaware, 19709, United States</div>
        <div class="text dem precinct">Precinct Margin: D+28%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=39.5109104765113,-75.67987301260885&ll=39.5109104765113,-75.67987301260885&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/989.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-991">Voter 991's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 991 voted for Biden</div> -->
        <div class="text coordinate"> 35&deg;29'35"N 80&deg;50'44"W</div>
        <div class="text address"><i>approx.</i> 577, Greenway Street, Downtown, Davidson, Mecklenburg County, North Carolina, 28036, United States</div>
        <div class="text dem precinct">Precinct Margin: D+35%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=35.49310794422639,-80.845637000157&ll=35.49310794422639,-80.845637000157&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/990.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-992">Voter 992's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 992 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;19'40"N 75&deg;19'36"W</div>
        <div class="text address"><i>approx.</i> 246, East Broad Street, Telford, Bucks County, Pennsylvania, 18969, United States</div>
        <div class="text gop precinct">Precinct Margin: R+4%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.32802317486651,-75.32692274306307&ll=40.32802317486651,-75.32692274306307&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/991.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-993">Voter 993's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 993 voted for Biden</div> -->
        <div class="text coordinate"> 33&deg;50'21"N 84&deg;21'14"W</div>
        <div class="text address"><i>approx.</i> 1019, Crane Road Northeast, Atlanta, Fulton County, Georgia, 30324, United States</div>
        <div class="text dem precinct">Precinct Margin: D+40%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=33.839342349999995,-84.35406698336683&ll=33.839342349999995,-84.35406698336683&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/992.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-994">Voter 994's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 994 voted for Biden</div> -->
        <div class="text coordinate"> 42&deg;45'38"N 71&deg;3'58"W</div>
        <div class="text address"><i>approx.</i> 263, Salem Street, South Groveland, Haverhill, Essex County, Massachusetts, 01835, United States</div>
        <div class="text dem precinct">Precinct Margin: D+13%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=42.76069215,-71.06624971510627&ll=42.76069215,-71.06624971510627&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/993.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-995">Voter 995's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 995 voted for Biden</div> -->
        <div class="text coordinate"> 37&deg;20'38"N 108&deg;35'51"W</div>
        <div class="text address"><i>approx.</i> Bravo! Cleaning & Restoration, 723, West 4th Street, Cortez, Montezuma County, Colorado, 81321-3299, United States</div>
        <div class="text gop precinct">Precinct Margin: R+14%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=37.3439021,-108.5975782&ll=37.3439021,-108.5975782&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/994.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-996">Voter 996's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 996 voted for Biden</div> -->
        <div class="text coordinate"> 27&deg;17'56"N 80&deg;20'40"W</div>
        <div class="text address"><i>approx.</i> 267, Southeast Ray Avenue, Port Saint Lucie, Saint Lucie County, Florida, 34983, United States</div>
        <div class="text gop precinct">Precinct Margin: R+2%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=27.298908218921127,-80.34467381018028&ll=27.298908218921127,-80.34467381018028&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/995.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-997">Voter 997's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 997 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;35'13"N 73&deg;40'6"W</div>
        <div class="text address"><i>approx.</i> 145, West Olive Street, City of Long Beach, Nassau County, New York, 11561, United States</div>
        <div class="text dem precinct">Precinct Margin: D+42%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.58714145,-73.66839585390883&ll=40.58714145,-73.66839585390883&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/996.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-998">Voter 998's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 998 voted for Biden</div> -->
        <div class="text coordinate"> 40&deg;46'57"N 73&deg;31'52"W</div>
        <div class="text address"><i>approx.</i> 20, 18th Street, Birchwood at Jericho, Hicksville, Town of Oyster Bay, Nassau County, New York, 11753, United States</div>
        <div class="text dem precinct">Precinct Margin: D+9%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=40.7825565,-73.5311682409858&ll=40.7825565,-73.5311682409858&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/997.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-999">Voter 999's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 999 voted for Biden</div> -->
        <div class="text coordinate"> 36&deg;48'47"N 76&deg;44'25"W</div>
        <div class="text address"><i>approx.</i> 47, Virginia Avenue, Windsor, Isle of Wight County, Virginia, 23487, United States</div>
        <div class="text gop precinct">Precinct Margin: R+36%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=36.81326886117305,-76.7405462379912&ll=36.81326886117305,-76.7405462379912&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/998.png"/>
            </a>
        </div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-1000">Voter 1000's Neighborhood</h3>

        <!-- <div class="text dem voter">Voter 1000 voted for Biden</div> -->
        <div class="text coordinate"> 29&deg;4'5"N 81&deg;2'17"W</div>
        <div class="text address"><i>approx.</i> Port Orange Fire / Rescue Station 4, Airport Road, Sabal Creek, Port Orange, Volusia County, Florida, 32128, United States</div>
        <div class="text gop precinct">Precinct Margin: R+26%</div>
    </td>
    </tr>
<tr>
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q=29.06810285,-81.03820628583333&ll=29.06810285,-81.03820628583333&z=8" target="_blank">
                <image src="/resources/2022-06-20/out/999.png"/>
            </a>
        </div>
    </td>
    </tr>
    </tr>
</table>