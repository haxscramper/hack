SELECT commits.period AS commmit_period,
       changed_ranges.period AS change_period,
       sum(changed_ranges.`end` - changed_ranges.`begin` + 1) AS line_count,
       commits.hash AS hash
  FROM commits
 INNER JOIN FILE
    ON commits.id == file.commit_id
 INNER JOIN changed_ranges
    ON file.id == changed_ranges.file
 GROUP BY commits.period,
          changed_ranges.period;
