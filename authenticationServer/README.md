Authentication server:
  TGT  (
      Name
      TGS Name
      timestamp
      clienty IP
      lifetime of TGT
      TGS Session KEY
    )
    
  RTGTPART2  (
      TGS name
      Timestamp
      Lifetime
      TGS Session KEY
    ) 
   
  To store in DB  
      userid and ClientSecretKEY
      FileServer  - secretkey
  lookupUserIDinDB:: userID 
  generateSessionkey:: 
  encryptmessage::  Message -> key 
  decryptmessage::  Message -> key 
  checklifetime
  Client
  data ( 
      username/ID
      TGT Server name
      IP adress
      lifetime
  )
  generateSecretkey
  decryptmessage:: Message -> key