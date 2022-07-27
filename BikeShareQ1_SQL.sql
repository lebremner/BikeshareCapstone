Select *
From Q1_21
-----------------------------------------------------------------------------------------------
--Calcluate trip duration, add to table
Select started_at, ended_at, DATEDIFF(MINUTE, started_at, ended_at) as trip_duration
From Q1_21

ALTER TABLE Q1_21
Add trip_duration int;

Update Q1_21
SET trip_duration =  DATEDIFF(MINUTE, started_at, ended_at);

Select *
From Q1_21

------------------------------------------------------------------------------------------------
--Add day of the week column
SELECT started_at, DATENAME(weekday, started_at) as day_of_week
FROM Q1_21;

ALTER TABLE Q1_21
Add day_of_week nvarchar(50);

UPDATE Q1_21
SET day_of_week =  DATENAME(weekday, started_at)

Select *
From Q1_21

-----------------------------------------------------------------------------------------------
--splitting out start date from start time
Select *
From Q1_21

Select started_at, CONVERT (Date, started_at) as start_date
From Q1_21

ALTER TABLE Q1_21
Add start_date date;

UPDATE Q1_21
SET start_date =   CONVERT (Date, started_at)

SELECT * 
FROM Q1_21

-------------------------------------------------------------------------------------------------------
--Daily Ride Count grouped by membership type

SELECT start_date, member_casual, count(start_date) as daily_ride_count
FROM Q1_21
GROUP BY start_date, member_casual
ORDER BY start_date

-----------------------------------------------------------------------------------------------------------
--Average daily trip duration by membership type 

SELECT start_date, member_casual,  AVG(trip_duration) as avg_trip_duration
FROM Q1_21
GROUP BY start_date, member_casual
ORDER BY start_date 


---------------------------------------------------------------------------------------------------------------
--Day of the week count my membership type
SELECT * 
FROM Q1_21

SELECT day_of_week, member_casual, COUNT(day_of_week) as weekday_count
FROM Q1_21
GROUP BY day_of_week, member_casual
ORDER BY day_of_week




-----------------------------------------------------------------------------------------------------------------
--Top 20 Stations by Membership Type
SELECT TOP 20 start_station_name, member_casual, COUNT(start_station_name) as popular_stations
FROM Q1_21
WHERE start_station_name NOT LIKE 'NA' AND member_casual LIKE 'member'
GROUP BY start_station_name, member_casual
ORDER BY popular_stations DESC

SELECT TOP 20 start_station_name, member_casual, COUNT(start_station_name) as popular_stations
FROM Q1_21
WHERE start_station_name NOT LIKE 'NA' AND member_casual LIKE 'casual'
GROUP BY start_station_name, member_casual
ORDER BY popular_stations DESC


------------------------------------------------------------------------------------------------------------------
--Average of latitude and longitude, smoothed for visualization
SELECT start_station_name,
	AVG (start_lat) as start_lat_avg,
	AVG(start_lng) as start_lng_avg
FROM Q1_21
GROUP BY start_station_name

----------------------------------------------------------------
--Views for later visualization

CREATE VIEW daily_ride_count
AS
SELECT start_date, member_casual, count(start_date) as daily_ride_count
FROM Q1_21
GROUP BY start_date, member_casual

CREATE VIEW avg_trip_duration
AS
SELECT start_date, member_casual,  AVG(trip_duration) as avg_trip_duration
FROM Q1_21
GROUP BY start_date, member_casual
--ORDER BY start_date 

CREATE VIEW day_week
AS
SELECT day_of_week, member_casual, COUNT(day_of_week) as weekday_count
FROM Q1_21
GROUP BY day_of_week, member_casual
--ORDER BY day_of_week

