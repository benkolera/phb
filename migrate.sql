BEGIN;

CREATE TABLE task (
  id       SERIAL       NOT NULL PRIMARY KEY
, person   BIGINT       NOT NULL REFERENCES person(id)
, name     VARCHAR(255) NOT NULL
, start    DATE         NOT NULL
, finish   DATE
, project  BIGINT                 REFERENCES project(id)
, event    BIGINT                 REFERENCES event(id)
, backlog  BIGINT                 REFERENCES backlog(id)
, action   BIGINT                 REFERENCES action(id)
, category BIGINT                 REFERENCES work_category(id)
);

CREATE TEMPORARY TABLE new_tasks (
  task      INTEGER
, time_log  INTEGER
);

INSERT INTO new_tasks (SELECT nextval('task_id_seq'), id FROM time_log);

INSERT INTO task
( SELECT task,person,
  coalesce(project.name,backlog.name,event.name,action.name,work_category.name,time_log."desc")
  ,day,day,project,event,backlog,action,category
  FROM new_tasks
    JOIN time_log           ON (time_log = time_log.id)
    LEFT JOIN project       ON (project = project.id)
    LEFT JOIN backlog       ON (backlog = backlog.id)
    LEFT JOIN event         ON (event = event.id)
    LEFT JOIN action        ON (action = action.id)
    LEFT JOIN work_category ON (category = work_category.id)
);

ALTER TABLE time_log DROP COLUMN project;
ALTER TABLE time_log DROP COLUMN backlog;
ALTER TABLE time_log DROP COLUMN event;
ALTER TABLE time_log DROP COLUMN action;
ALTER TABLE time_log DROP COLUMN category;

ALTER TABLE time_log ADD COLUMN task BIGINT REFERENCES task(id);

UPDATE time_log SET task = (SELECT task FROM new_tasks WHERE time_log = time_log.id);

ALTER TABLE time_log ALTER COLUMN task SET NOT NULL;

--COMMIT;
