import Debug "mo:base/Debug";
import Text "mo:base/Text";
import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Array "mo:base/Array";
import Time "mo:base/Time";
import Hash "mo:base/Hash";
import Principal "mo:base/Principal";
import Iter "mo:base/Iter";

actor {
    // Hayvan veri yapısı
    type Animal = {
        id : Text;
        name : Text;
        species : Text;
        breed : Text;
        age : Nat;
        healthStatus : Text;
        shelterLocation : Text;
        adoptionStatus : {
            #Available;
            #Pending;
            #Adopted;
        };
        rescueDate : Time.Time;
        medicalHistory : [Text];
        specialNeeds : ?Text;
    };

    // Kullanıcı veri yapısı
    type User = {
        id : Text;
        name : Text;
        contact : Text;
        adoptedAnimals : [Text];
        volunteerStatus : Bool;
    };

    // Hayvan kaydı için HashMap
    let animalRegistry = HashMap.HashMap<Text, Animal>(10, Text.equal, Text.hash);
    
    // Kullanıcı kaydı için HashMap
    let userRegistry = HashMap.HashMap<Text, User>(10, Text.equal, Text.hash);

    // Sahiplendirme talepleri için HashMap
    let adoptionRequests = HashMap.HashMap<Text, Text>(10, Text.equal, Text.hash);

    // Hayvan ekleme fonksiyonu
    public func addAnimal(
        name : Text, 
        species : Text, 
        breed : Text, 
        age : Nat,
        healthStatus : Text,
        shelterLocation : Text,
        medicalHistory : [Text],
        specialNeeds : ?Text
    ) : async Text {
        let id = generateUniqueId();
        let newAnimal : Animal = {
            id = id;
            name = name;
            species = species;
            breed = breed;
            age = age;
            healthStatus = healthStatus;
            shelterLocation = shelterLocation;
            adoptionStatus = #Available;
            rescueDate = Time.now();
            medicalHistory = medicalHistory;
            specialNeeds = specialNeeds;
        };
        
        animalRegistry.put(id, newAnimal);
        return id;
    };

    // Kullanıcı kayıt fonksiyonu
    public func registerUser(
        name : Text, 
        contact : Text, 
        volunteerStatus : Bool
    ) : async Text {
        let id = generateUniqueId();
        let newUser : User = {
            id = id;
            name = name;
            contact = contact;
            adoptedAnimals = [];
            volunteerStatus = volunteerStatus;
        };
        
        userRegistry.put(id, newUser);
        return id;
    };

    // Hayvan sahiplendirme talebi
    public func requestAdoption(userId : Text, animalId : Text) : async Bool {
        switch (userRegistry.get(userId), animalRegistry.get(animalId)) {
            case (?(user), ?(animal)) {
                if (animal.adoptionStatus == #Available) {
                    // Hayvanın durumunu beklemede olarak güncelle
                    let updatedAnimal = {
                        animal with 
                        adoptionStatus = #Pending
                    };
                    animalRegistry.put(animalId, updatedAnimal);
                    
                    // Sahiplendirme talebini kaydet
                    adoptionRequests.put(animalId, userId);
                    return true;
                };
                return false;
            };
            case _ { return false; }
        }
    };

    // Sahiplendirmeyi sonlandırma
    public func confirmAdoption(animalId : Text) : async Bool {
        switch (adoptionRequests.get(animalId), animalRegistry.get(animalId)) {
            case (?(userId), ?(animal)) {
                switch (userRegistry.get(userId)) {
                    case (?(user)) {
                        // Hayvanın durumunu sahiplenildi olarak güncelle
                        let updatedAnimal = {
                            animal with 
                            adoptionStatus = #Adopted
                        };
                        animalRegistry.put(animalId, updatedAnimal);

                        // Kullanıcının sahiplendirdiği hayvanları güncelle
                        let updatedUser = {
                            user with 
                            adoptedAnimals = Array.append(user.adoptedAnimals, [animalId])
                        };
                        userRegistry.put(userId, updatedUser);

                        // Sahiplendirme talebini sil
                        adoptionRequests.delete(animalId);
                        return true;
                    };
                    case _ { return false; }
                }
            };
            case _ { return false; }
        }
    };

    // Sahiplendirilebilir hayvanları listeleme
    public query func getAvailableAnimals() : async [Animal] {
        let availableAnimals = Array.mapFilter<(Text, Animal), Animal>(
            Iter.toArray(animalRegistry.entries()),
            func((_, animal)) {
                if (animal.adoptionStatus == #Available) {
                    ?animal
                } else {
                    null
                }
            }
        );
        return availableAnimals;
    };

    // Benzersiz ID üretme fonksiyonu
    func generateUniqueId() : Text {
        return debug_show(Time.now());
    };

    // Hayvan detaylarını alma
    public query func getAnimalDetails(animalId : Text) : async ?Animal {
        animalRegistry.get(animalId)
    };
}
