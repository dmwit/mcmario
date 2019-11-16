create table version (version int not null);
insert into version (version) values (0);

create table player
	( id int8 primary key
	, component int4 not null
	, ignore_null_username bool not null
	, rating numeric(16,8) not null
	, username text
	, hash bytea not null
	);
create unique index player_username on player(username);

create table component
	( id serial primary key
	, leader int8 not null references player(id)
	);
alter table player add foreign key (component) references component(id);

create table session
	( player int8 not null references player(id)
	, expiration timestamptz not null
	, id bytea primary key
	);
create index session_player on session(player);
create index session_expiration on session(expiration);

create table speed
	( id int2 primary key
	, description varchar(3) not null
	);
insert into speed (id, description) values
	(0, 'low'),
	(1, 'med'),
	(2, 'hi');

create table level (id int2 primary key);
insert into level (id) values (0), (1), (2), (3), (4), (5), (6), (7), (8), (9), (10), (11), (12), (13), (14), (15), (16), (17), (18), (19), (20);

create table status
	( id int2 primary key
	, description text not null
	);
insert into status (id, description) values
	(0, 'unacknowledged'),
	(1, 'accepted'),
	(2, 'rejected');

create table game_settings
	( id serial primary key
	, player int8 not null references player(id)
	, speed int2 not null references speed(id)
	, level int2 not null references level(id)
	, status int2 not null references status(id)
	, ignore_status_mismatch bool not null
	);

create table game
	( id int8 primary key
	, date timestamptz not null
	, status int2 not null references status(id)
	, winner int8 not null references game_settings(id)
	, loser int8 not null references game_settings(id)
	);
