iRetCode, sMsg, d = Pbl_wind.read_raw("..\\SonicLib\\ParcoNord\\20121025.12.csv")
if iRetCode != 0
    print("Error - Return code = ")
    println(iRetCode)
    println(sMsg)
    return 1
end
n = 30*60
e = Pbl_wind.average_raw(d, n)
println(e)
