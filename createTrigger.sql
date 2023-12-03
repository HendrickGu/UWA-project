CREATE TRIGGER updateRentalCost
AFTER UPDATE OF dateBack ON rentalContract
FOR EACH ROW
WHEN NEW.dateBack IS NOT NULL AND OLD.dateBack IS NULL
BEGIN
UPDATE rentalContract
SET rentalCost = (
 SELECT ROUND((baseCost + (dailyCost * (julianday(NEW.dateBack) - julianday(OLD.dateOut) + 1))),2)
 FROM PhoneModel
 JOIN Phone ON Phone.modelNumber = PhoneModel.modelNumber AND Phone.modelName = PhoneModel.modelName
 WHERE Phone.IMEI = NEW.IMEI
)
WHERE customerId = New.customerId AND IMEI =NEW.IMEI;
END;