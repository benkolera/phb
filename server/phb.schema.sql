BEGIN;

DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

CREATE TABLE customer (
  id   SERIAL  NOT NULL PRIMARY KEY
, name VARCHAR NOT NULL
);

CREATE TABLE system (
  id   SERIAL  NOT NULL PRIMARY KEY
, name VARCHAR NOT NULL
);

CREATE TABLE effort_category (
  id                     SERIAL  NOT NULL PRIMARY KEY
, name                   VARCHAR NOT NULL
, expected_person_hours  INTEGER NOT NULL
);

CREATE TABLE person (
  id                 SERIAL  NOT NULL PRIMARY KEY
, name               VARCHAR NOT NULL
, email              VARCHAR NOT NULL
, customer_id        INTEGER          REFERENCES customer(id)
, receives_heartbeat BOOLEAN NOT NULL
, logs_time          BOOLEAN NOT NULL
);

CREATE TYPE work_item_type AS ENUM('project','backlog','support_category','event');

CREATE TABLE work_item (
  id                        SERIAL         NOT NULL PRIMARY KEY
, name                      VARCHAR        NOT NULL
, notes                     TEXT           NOT NULL                         DEFAULT ''
, type                      work_item_type NOT NULL
);

CREATE TABLE work_item_involves (
  work_item_id INTEGER NOT NULL REFERENCES work_item(id)
, person_id    INTEGER NOT NULL REFERENCES person(id)
, description  VARCHAR NOT NULL
, PRIMARY KEY (work_item_id,person_id)
);

CREATE TABLE work_item_customer (
  work_item_id    INTEGER      NOT NULL REFERENCES work_item(id)
, customer_id     INTEGER      NOT NULL REFERENCES customer(id)
, cost_percentage NUMERIC(5,2)
, description     VARCHAR      NOT NULL
, PRIMARY KEY (work_item_id,customer_id)
);

CREATE TABLE work_item_effort (
  work_item_id       INTEGER      NOT NULL REFERENCES work_item(id)
, effort_category    INTEGER      NOT NULL REFERENCES effort_category(id)
, effort_uncertainty NUMERIC(5,2) NOT NULL
);

CREATE TABLE work_item_systems (
  work_item_id  INTEGER      NOT NULL REFERENCES work_item(id)
, system_id     INTEGER      NOT NULL REFERENCES system(id)
, PRIMARY KEY (work_item_id,system_id)
);

CREATE TYPE project_status AS ENUM('Analysis','Prototyping','Development','Stalled','Testing','Ready to Deploy','Completed');

CREATE TABLE project (
  work_item_id        INTEGER        NOT NULL PRIMARY KEY REFERENCES work_item(id)
, status              project_status NOT NULL
, start_date          DATE           NOT NULL
, planned_finish_date DATE
, finish_date  DATE
);

CREATE TYPE milestone_status AS ENUM('OK','At Risk','Behind');

CREATE TABLE project_milestones (
  milestone_id  SERIAL           NOT NULL PRIMARY KEY
, work_item_id  INTEGER          NOT NULL REFERENCES work_item(id)
, name          VARCHAR          NOT NULL
, expected_date DATE             NOT NULL
, actual_date   DATE
, status        milestone_status NOT NULL
);

CREATE TYPE backlog_status AS ENUM('Commercial Analysis','Technical Analysis','Ready to Dev');

CREATE TABLE backlog (
  work_item_id       INTEGER        NOT NULL REFERENCES work_item(id)
, status             backlog_status NOT NULL 
, expected_start     DATE
, priority           INTEGER        NOT NULL
);

CREATE TABLE event (
  work_item_id  INTEGER     NOT NULL REFERENCES work_item(id)
, planned       BOOLEAN     NOT NULL
, impact        TEXT        NOT NULL
, start_time    TIMESTAMPTZ
, finish_time   TIMESTAMPTZ
);

CREATE TABLE task (
  id           SERIAL  NOT NULL PRIMARY KEY
, name         VARCHAR NOT NULL
, work_item_id INTEGER NOT NULL REFERENCES work_item(id)
);

CREATE TABLE snapshot (
  id          SERIAL NOT NULL PRIMARY KEY
, start_date  DATE   NOT NULL
, finish_date DATE   NOT NULL
, data        JSON   NOT NULL
);


COMMIT;
