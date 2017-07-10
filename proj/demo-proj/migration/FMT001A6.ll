
# Partition: FMT001A6_
Data_Names TEST1A = TEST1A
Data_Types TEST1A = BYTE_p
Data_Lengths TEST1A = 4
Data_Names TEST1B = TEST1B
Data_Types TEST1B = BYTE_p
Data_Lengths TEST1B = 4
Data_Names TEST2 = TEST2
Data_Types TEST2 = BYTE_p
Data_Lengths TEST2 = 4
Data_Names WREC2 = WREC2
Data_Types WREC2 = BYTE_p
Data_Lengths WREC2 = 80
Data_Names WTAB1 = WTAB1
Data_Types WTAB1 = BYTE_p
Data_Lengths WTAB1 = 16

# Partition: WDS1
Data_Names WDS1 = WDS1
Data_Types WDS1 = struct_p
Data_Lengths WDS1 = 16
Data_Names WDS1F1 = WDS1.WDS1F1
Data_Types WDS1.WDS1F1 = BYTE_p
Data_Lengths WDS1.WDS1F1 = 4
Data_Fields WDS1 0 4 = WDS1F1
Data_Names WDS1F2 = WDS1.WDS1F2
Data_Types WDS1.WDS1F2 = BYTE_p
Data_Lengths WDS1.WDS1F2 = 6
Data_Fields WDS1 4 6 = WDS1F2
Data_Names WDS1F3 = WDS1.WDS1F3
Data_Types WDS1.WDS1F3 = BYTE_p
Data_Lengths WDS1.WDS1F3 = 3
Data_Fields WDS1 10 3 = WDS1F3
Data_Names WDS1F4 = WDS1.WDS1F4
Data_Types WDS1.WDS1F4 = BYTE_p
Data_Lengths WDS1.WDS1F4 = 3
Data_Fields WDS1 13 3 = WDS1F4

# Partition: WDS2
Data_Names WDS2 = WDS2
Data_Types WDS2 = struct_p
Data_Lengths WDS2 = 28
Data_Names WDS2F1 = WDS2.WDS2F1
Data_Types WDS2.WDS2F1 = BYTE_p
Data_Lengths WDS2.WDS2F1 = 4
Data_Fields WDS2 0 4 = WDS2F1
Data_Names WDS2F2 = WDS2.WDS2F2
Data_Types WDS2.WDS2F2 = BYTE_p
Data_Lengths WDS2.WDS2F2 = 6
Data_Fields WDS2 8 6 = WDS2F2
Data_Names WDS2F3 = WDS2.WDS2F3
Data_Types WDS2.WDS2F3 = BYTE_p
Data_Lengths WDS2.WDS2F3 = 3
Data_Fields WDS2 18 3 = WDS2F3
Data_Names WDS2F4 = WDS2.WDS2F4
Data_Types WDS2.WDS2F4 = BYTE_p
Data_Lengths WDS2.WDS2F4 = 3
Data_Fields WDS2 25 3 = WDS2F4
# End of file
