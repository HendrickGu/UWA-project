CREATE VIEW CustomerSummary AS
SELECT
  rentalContract.customerId,
  Phone.modelName,
  SUM(julianday(rentalContract.dateBack) - julianday(rentalContract.dateOut) + 1) AS daysRented,
  IIF(
    CAST(substr(rentalContract.dateBack, 6, 2) AS INTEGER) <= 6,
    (CAST(substr(rentalContract.dateBack, 1, 4) AS INTEGER) - 1) || '/' || substr(CAST(substr(rentalContract.dateBack, 1, 4) AS INTEGER), 3, 2),
    CAST(substr(rentalContract.dateBack, 1, 4) AS INTEGER) || '/' || substr(CAST(substr(rentalContract.dateBack, 1, 4) AS INTEGER) + 1, 3, 2)
  ) AS taxYear,
  SUM(rentalContract.rentalCost) AS rentalCost
FROM rentalContract
LEFT JOIN Phone ON rentalContract.IMEI = Phone.IMEI
WHERE rentalContract.dateBack IS NOT NULL
GROUP BY rentalContract.customerId, Phone.modelName, taxYear;
