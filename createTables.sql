CREATE TABLE PhoneModel (
  modelNumber TEXT,
  modelName TEXT,
  storage INTEGER,
  colour TEXT,
  baseCost REAL,
  dailyCost REAL,
  PRIMARY KEY (modelNumber,modelName)
);

CREATE TABLE Customer (
  customerId INTEGER PRIMARY KEY,
  customerName TEXT,
  customerEmail TEXT
);

CREATE TABLE Phone (
  modelNumber TEXT,
  modelName TEXT,
  IMEI TEXT PRIMARY KEY,
  FOREIGN KEY (modelNumber, modelName) REFERENCES PhoneModel(modelNumber, modelName)
  CHECK (
    LENGTH(IMEI) = 15 AND
    (
      (CAST(SUBSTR(IMEI, 1, 1) AS INTEGER) +
       CAST(SUBSTR(IMEI, 3, 1) AS INTEGER) +
       CAST(SUBSTR(IMEI, 5, 1) AS INTEGER) +
       CAST(SUBSTR(IMEI, 7, 1) AS INTEGER) +
       CAST(SUBSTR(IMEI, 9, 1) AS INTEGER) +
       CAST(SUBSTR(IMEI, 11, 1) AS INTEGER) +
       CAST(SUBSTR(IMEI, 13, 1) AS INTEGER) +
       CAST(SUBSTR(IMEI, 15, 1) AS INTEGER) +
       (CAST(SUBSTR(IMEI, 2, 1) AS INTEGER) * 2 % 10 + CAST(SUBSTR(IMEI, 2, 1) AS INTEGER) * 2 / 10) +
       (CAST(SUBSTR(IMEI, 4, 1) AS INTEGER) * 2 % 10 + CAST(SUBSTR(IMEI, 4, 1) AS INTEGER) * 2 / 10) +
       (CAST(SUBSTR(IMEI, 6, 1) AS INTEGER) * 2 % 10 + CAST(SUBSTR(IMEI, 6, 1) AS INTEGER) * 2 / 10) +
       (CAST(SUBSTR(IMEI, 8, 1) AS INTEGER) * 2 % 10 + CAST(SUBSTR(IMEI, 8, 1) AS INTEGER) * 2 / 10) +
       (CAST(SUBSTR(IMEI, 10, 1) AS INTEGER) * 2 % 10 + CAST(SUBSTR(IMEI, 10, 1) AS INTEGER) * 2 / 10) +
       (CAST(SUBSTR(IMEI, 12, 1) AS INTEGER) * 2 % 10 + CAST(SUBSTR(IMEI, 12, 1) AS INTEGER) * 2 / 10) +
       (CAST(SUBSTR(IMEI, 14, 1) AS INTEGER) * 2 % 10 + CAST(SUBSTR(IMEI, 14, 1) AS INTEGER) * 2 / 10)
    ) % 10 = 0
  )
));

CREATE TABLE rentalContract (
    customerId INTEGER,
    IMEI  TEXT,
    dateOut TEXT,
    dateBack TEXT,
    rentalCost REAL,
    PRIMARY KEY (customerId, IMEI, dateOut)
    FOREIGN KEY (customerId) REFERENCES Customer(customerId)
    FOREIGN KEY (IMEI) REFERENCES Phone(IMEI) ON DELETE SET NULL
);
