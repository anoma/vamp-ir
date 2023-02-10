// Note: This is a custom, small poseidon perm for testing in vamp-ir

// Numbers randomly generated using Grain LFSR (See Addendum F in paper)
def roundConstant1 = 43442449452518235879993862661946600569203605360132186721265738401475259657628;
def roundConstant2 = 38818623264151656284402567202123467327825563240790483432694745605906909120401;
def roundConstant3 = 26638374687280120167736375685698144891411412776658539716454891393834356195680;
def roundConstant4 = 28654699052765126438131722414552290608260414542086301986368153536295571554051;
def roundConstant5 = 4673117996383411655291615127700302269439722728655115894864187039578662128768;
def roundConstant6 = 25260779001792909557265359215330406175764180823853004747831496139755630235066;
def roundConstant7 = 27130326702329958489522658441276932407635020592756172972257498732043653865799;
def roundConstant8 = 12564067668209349276283384465145346241181216457402522276937509096026837311801;
def roundConstant9 = 12457595853515776350319685685199873394324458960858392705123939596504715131642;
def roundConstant10 = 38919266803308392857118122572938195799193826101132364566857996258880105628916;
def roundConstant11 = 28955807644501396005649604365234512737645487450715966924080434068719145351368;
def roundConstant12 = 49666711218392918511659224489910707133270804652454781674649756387483600156490;
def roundConstant13 = 50281616103005719853919952104715687201202491918177084007453669389803420981602;
def roundConstant14 = 7021491147764236528909960826939051174366311323983178084967601185312929694923;
def roundConstant15 = 11889523476110819382625985788414919504568394215686016258683131308394091342599;
def roundConstant16 = 26361316010466203164840311502027656066828416115827411609670382870574102001222;
def roundConstant17 = 21962215021124078542118831125844969792774971363108912431438866706664881154796;
def roundConstant18 = 35956213739477416645439846651935651598827112130514032666346259111871930638527;
def roundConstant19 = 29132679442423225262592922230563065979263431095432955892378775388781959812406;
def roundConstant20 = 12788934897126067000279359922228768747378751222249897393216444788476593287532;
def roundConstant21 = 34029752697103562992278041145992441605202680551213217857794959853164187784842;
def roundConstant22 = 44141865561047358391069259944379528002257869735687521745507343538485286299318;
def roundConstant23 = 1070115429576833168949622510376242502329425138630059168225451107746486293916;
def roundConstant24 = 2841613892997380883389831533344576461160221392034790255262252458441253329295;
def roundConstant25 = 28957839799650451173740453288308409001523074046602537045614700623854902663796;
def roundConstant26 = 9617717254885230961015441226467176456094572979949541582761042127880203337185;
def roundConstant27 = 7361373494394756955870238908822181923126607795531252132445453556710501336835;
def roundConstant28 = 8026716625243082753437272989268373977053376976785080169142774025743446280268;
def roundConstant29 = 44212033403628207719926237329816854239502672349681775028206214341660166135889;
def roundConstant30 = 45796065751150975497657245725147258225651579663401159855612945103412295481746;
def roundConstant31 = 51716969692879618554106278307435255663842843905767717543157299719374587872221;
def roundConstant32 = 50846234152178174257540225861112093623428145719553049983275271918068658082001;
def roundConstant33 = 9188523414124605942865432027183352592617735262250618786942829261664924549956;
def roundConstant34 = 39703025378003717640992792594327156722457615941410656046322518616559959216906;
def roundConstant35 = 35831386477205500236523572973700656347530726071501514713471840861197351950829;
def roundConstant36 = 21008359709966643294210457524147398340474550748967817586008732096993916055534;
def roundConstant37 = 36765710335514792332417494171848355882623089395447619903010832114338425592312;
def roundConstant38 = 19119020267281389883176440612542761083973451587922463270003638314191279150609;
def roundConstant39 = 22231176105027847893989620425424374589707138921517660945068539030215196545116;
def roundConstant40 = 30068284822828210594427892878901467550638169508518219123696148797485430640632;
def roundConstant41 = 11943732477482108317156116161915603193331192363056246321031600088946054606116;
def roundConstant42 = 52269522774829150845708285787605496405379767357925053097414453167957236031327;
def roundConstant43 = 26323459875176776710863576123306953428486837115100442231736016658448551899631;
def roundConstant44 = 1053713296442671869891673374356487574255269297622829469949245520588366047563;
def roundConstant45 = 28101793702058620284372959300405871622969254742961810085817535493873286493589;
def roundConstant46 = 24883184194141864315020060955596288123763008184127480621782730368531347655349;
def roundConstant47 = 10370212985826458596495255006865171668366184380642988472272459075625049678993;
def roundConstant48 = 48988628316805292640374381755131621807770924568276924672637853195273901018279;
def roundConstant49 = 24476903368481342360241853688416795602944381280039987220244618248719825824955;
def roundConstant50 = 48653001145611399681687596665441469800856166859787795253181362329315578187375;
def roundConstant51 = 25898611760334163359926961869551248526414372720792187826661524976776443215354;
def roundConstant52 = 13210123902828422333998743914841589641331512713869613652340895323367254865638;
def roundConstant53 = 48193436886780015323974427033523253455499535347816726737585969338027538727758;
def roundConstant54 = 21753206819617155325647313695466588584096817294753646789874316985527318312117;
def roundConstant55 = 16782104962802141778191874317494166338738358687090198658449224312640000566761;
def roundConstant56 = 28218662783462618671212779631479456423267562297528411208122081938343848683870;
def roundConstant57 = 44847034113984058900115055480550711860832483605883266113670360965887800814382;
def roundConstant58 = 26251201889250631764412336183004817322342102249669281582058069258894918593385;
def roundConstant59 = 35808966018459497837077852589586703940389037703034350777275912620527006671966;
def roundConstant60 = 1057922684538010116172993379370864691786150258631379409794313697636605923196;
def roundConstant61 = 35712638888336832515882766131993924756891876991235490961732959100507310674384;
def roundConstant62 = 31772965896852285815054455015651452932013556502699409264486536659083643789919;
def roundConstant63 = 49310756871626442445335345607548216332817993980590176455692273449738795065644;
def roundConstant64 = 39830932259356632336933392618009774853807852648566834755503331297428086496795;
def roundConstant65 = 12041385844682317015195496280051397988946083599700854449566372489564106607911;
def roundConstant66 = 49159233650624772267808878717034892796925156243516501197126923439958478908898;
def roundConstant67 = 4248046513067851295436228455045437803624038929905191192340983242456978168234;
def roundConstant68 = 35161412792194697748984607858985362531322297410179691435610447273659932178810;
def roundConstant69 = 35818297261929045836807106270662279562857382519127190604764665922000147299343;
def roundConstant70 = 31018978578883147944215838515574902468632756355797563742046691424808261870519;
def roundConstant71 = 36097021951895206833016510536748434075061715741789997199310242442857692408116;
def roundConstant72 = 8713840451476382757775603926808506335757891067775675525532932441897885780338;
def roundConstant73 = 28347812678438442769577592678521503352586890850221584250649105222982550799982;
def roundConstant74 = 23770818225478042693410540216956548625390378683346940948228638937011261989088;
def roundConstant75 = 35266682850726092969331544627615169426146184365249594245114850105730435469064;
def roundConstant76 = 42759753802956225380473974700498909404965764949117973064457059296975548108111;
def roundConstant77 = 26907809642508032195332557587457198128977377130311269172836607429817508399353;
def roundConstant78 = 30620035593125279342376183342360407779566947346557430091154181010228469278550;
def roundConstant79 = 3228292134353856281187972948552201016151305195532247390279561910510146563633;
def roundConstant80 = 9665521541546477808015340802229531776320498719338808393280626147342487568913;
def roundConstant81 = 18129783205343014990334777093391380214330085010827257960971956948843394161030;
def roundConstant82 = 27390778766134012431577025658231258400685555406824298589095165249683257765610;
def roundConstant83 = 9217395132559030580439564307036184996657743328479769428232706943813525472250;
def roundConstant84 = 2866919907311872644253781016267764458669100464240600436735821935224767116955;
def roundConstant85 = 41531949385844684197070034120629210447558801766544637460603339001587610236352;
def roundConstant86 = 2417436402819486758485240269269412380664466920813345696910623401364641122851;
def roundConstant87 = 9779637248477151082246615728403287761723965245468516599035093341504719150838;
def roundConstant88 = 37138586190483346536502695350028174874831385208154313382406434963315538628031;
def roundConstant89 = 33601029292743094016004967709648496984356935495877106614415872253079588162425;
def roundConstant90 = 16624197148306109283709718600809385630416520350695261444335450652946990629846;
def roundConstant91 = 25126396213238581704627134932436670846111331388306099554957668987776132626572;
def roundConstant92 = 11231961061365728272353583873629325655534377788840778893212119257942536785441;
def roundConstant93 = 23954403364201852682294278214470882071496160744299204854288013661340003028353;
def roundConstant94 = 40771565331507462454722368608532288051960071581059206098181102970343217041061;
def roundConstant95 = 23118797589645081796936780839094573524688648751188024922399484934386885508277;
def roundConstant96 = 38716104213269923004159157739067405320038257942935925007530790369854012426888;
def roundConstant97 = 13001166358684425327550961858710208122120703774158446674133894455969002068794;
def roundConstant98 = 41611038753069286170823239804505050647719423524328714780039955876318517652865;
def roundConstant99 = 13933343754196174989239977827430804266420440547570785786886209272502883806604;
def roundConstant100 = 25636835721542504157258826859623683272593404760552176032354207420663021671431;
def roundConstant101 = 45356026255514884513084643065023401148691835951936702471921969331202514825227;
def roundConstant102 = 36607816855559759838909676694122688958218615817451387734516617391676550107292;
def roundConstant103 = 9525867916527142244691105287055094126115356412260740047021831637530785372859;
def roundConstant104 = 29583304946123226384901348783140399375608561167372814085461015896485836476875;
def roundConstant105 = 29119384794648831747718749814493902278671262761414871318135713609755635108559;
def roundConstant106 = 7538873172472250691026009581439070243650812676895124176512835273899520959357;
def roundConstant107 = 51162541314512304688634068050781248708117949176237577957908882503038980881261;
def roundConstant108 = 27962566932679493017801480745646509602218298874402721941572211526772287218590;
def roundConstant109 = 28690395722934161966330237512732599266030622693783521731425653581637369541813;
def roundConstant110 = 2978719447359080451783842242313633590409126777157377807021244900119669085893;
def roundConstant111 = 50606369243762202925616124845098722057775404141146524430609452167151152912635;
def roundConstant112 = 20084523131983087469081745520558107044068266913918143376012272194335287439501;
def roundConstant113 = 11974083939154324158448706997313378770836016539109518849451876858043218997124;
def roundConstant114 = 17656881946715625577969172171563844205712150204432718417958053156047554768059;
def roundConstant115 = 30412272911336919225584295224026038814121803164926305159288011974999298018554;
def roundConstant116 = 10826340703080341807596531638599733488416092107334271779232101461624929656296;
def roundConstant117 = 25895888787407852127757999779416977879868614407131246851309086085328705089254;
def roundConstant118 = 25032093976840074447999485427734412474683682432673631182962469727599446817518;
def roundConstant119 = 32615212404130931929380112917700138747415943996966396354328880433378616665364;
def roundConstant120 = 21469173340343970841459845658455664756079976209100354410481284739341889209665;
def roundConstant121 = 51311598222812668497842255567217607669818744208133557779606258581706176216626;
def roundConstant122 = 4461241018471760627723902283341341444929807019075881323951342095983041850463;
def roundConstant123 = 33917155775586976951917697742088999405841420746707561149016602788031420210497;
def roundConstant124 = 42544110436031285843383196033417142799799873133340878291497282602566095013228;
def roundConstant125 = 27266213765451137786634305040759324116972695864187238028059302706988322520466;
def roundConstant126 = 9521984501860931566426537220058433367077156194319774299926595202979342463665;
def roundConstant127 = 41051513815198666019002253752132927800014754229829143063206904056870299206533;
def roundConstant128 = 36740961629831639962182295431055607697596706216157115445659969646059187243538;
def roundConstant129 = 15906674692006170581314664964962338689488659607466248294351084479924341161047;
def roundConstant130 = 14398646246550439676048070663544031194079887851975122810622781626746435599903;
def roundConstant131 = 13169584942137699510980039489400130557535359489560142276293620242309925418934;
def roundConstant132 = 23246638445054692120508367691602734343001067321925742498053393516471520433967;
def roundConstant133 = 24511562140215296937769705235213109117797081559124405801376182071818190156001;
def roundConstant134 = 37533333423908300338023634088860872022574308566223577092201704997424599093068;
def roundConstant135 = 44580050090510247506727758916069121019409054030834064349492176211198950292034;
def roundConstant136 = 19009827802016349400542191936202142358596059950718958781035688924987863902530;
def roundConstant137 = 28163652032784102173234946317492650826010174052387978329218538009334403163912;
def roundConstant138 = 19071640992529312173831386197439709669677211464722652627158010255329067800330;
def roundConstant139 = 51350507286637915966133648419598428624640428402091906829131228424963072675452;
def roundConstant140 = 11302805933151632411685712269201959136995573495096030804579266644663232661428;
def roundConstant141 = 34238909777084277390399790859251826592708415202861362070717652600948038983837;
def roundConstant142 = 28106849069340903580003205820037349307580211556705463221611163174364687433832;
def roundConstant143 = 34837440904003231089914889081908702401271101708649215953563976793405519497027;
def roundConstant144 = 45827105691370012957162191248503756935786722944089488701030275856218430007687;
def roundConstant145 = 37354493678931381366306176437914206690505146885314188181586268580573028228652;
def roundConstant146 = 45794467834807470227868100693673704841668585788862027269586988632131719776762;
def roundConstant147 = 16459112710634061179774293156639249133514157782673168903176277224606025477612;
def roundConstant148 = 31214266664462092728400972462434340246350335209084847366074913232415251768383;
def roundConstant149 = 28235660681250505564529720040497365064758811718908071640091441048663461108020;
def roundConstant150 = 50223578316243661512608416273603111034513607966108939772849950247083665031754;
def roundConstant151 = 16060329485642118193229157426968554263159028415461573140515262209437735758393;
def roundConstant152 = 48834455713893484842530052039870685927204800317254518116477757860607814803231;
def roundConstant153 = 24797785810155042460531802842759931589513615920515345176665831429474649092871;
def roundConstant154 = 36050861006666707674921105750266829220622211894855471927225735976110090415924;
def roundConstant155 = 34888078538485656659607806007401914595309592780882446369857454893541214647634;
def roundConstant156 = 8527104592581106506745210372782652011020910526407894719172359312551219792643;
def roundConstant157 = 20391046493784199194635446077289616466844147283955720471431316411047570883203;
def roundConstant158 = 46093859946893165529696893207184316982155985773219717566841315327717003767529;
def roundConstant159 = 46375530080886227774240845602760816575643956912206237080325943154201743515793;
def roundConstant160 = 14821053822402096678443785387434160091652324534191182133618661546223397393781;
def roundConstant161 = 22911053123573710299655863373269894555646028499597653311181001627906862513849;
def roundConstant162 = 30745638401172582408965267860493914719827765920153184786539331909921326479354;
def roundConstant163 = 31700571863049123423486937231825269228001616338260531846080546045548763566374;
def roundConstant164 = 19005613289525320321385418414951314096655878880292430542248299490166359462456;
def roundConstant165 = 39281655383790583278888903267774158335226788078034896598223078030846232331060;
def roundConstant166 = 20974539553266421966221402311986419427649033446426455401693466742667700513706;
def roundConstant167 = 10160091032537602447738453200982087391543779634300183637650093724018513122809;
def roundConstant168 = 49057099197068658668449520101888623336786585100488249540241200221800217637436;
def roundConstant169 = 30872763812667204228785185641458670088994875383439189588065091017239731987644;
def roundConstant170 = 13240932891814984719182287659094480773135504983343571984858232924699654318281;
def roundConstant171 = 50338512286791520630248647676684698137426228930208077711086284662232940355864;
def roundConstant172 = 47272781827579474334034999737058211516043164296156011588607973756194300308805;
def roundConstant173 = 26276756046081006043089349719812491727511096320623971993892112933308152704680;
def roundConstant174 = 1870392192652210613245423878014471162617209507728017950418868393397460239501;
def roundConstant175 = 45642961941520528202589675239432568688155959361713347381130948016666744125906;
def roundConstant176 = 5614462209020534394064952234798050597462167070448841926288618612370734430021;
def roundConstant177 = 25477639427498785029128571168291148425942102169586570612966458284985436004214;
def roundConstant178 = 16212287826050213784889949438335734866075293777172320088265756353834024898408;
def roundConstant179 = 18511377833915420688745452392385418126144584673941314839220120952043775521378;
def roundConstant180 = 1320431888386710488076271607885411912884456190292878150178115681380796587809;
def roundConstant181 = 34750525081318843195559059447418825429290647842255709968265616236670265292383;
def roundConstant182 = 20175577005702768973875995618992700704254210612046145274088016783636545877136;

// A random matrix with secure statistical properties (See Subsection "The Linear Layer" within 2.3 in paper)
def mds_matrix = ((8726208921296798925631310318607209695614088076071597212995306396202789397949, 21600819551494588616518000719262524338328457524274834324922196256779363029781), (36874265958224745587784444294445025026623634033551730857227299017135699388012, 5244603709194028534826861280444557184390877317737313216883214922824913747688));

def sbox a = {
  a*a*a
};

def zipWith2 f (a1, a2) (b1, b2) = {
  (f a1 b1, f a2 b2)
};

def MVmult2 ((m1_1, m1_2), (m2_1, m2_2)) (a1, a2) = {
  ((m1_1*a1)+(m1_2*a2),
   (m2_1*a1)+(m2_2*a2))
};

def map2 f (a1, a2) = {
  (f a1, f a2)
};

def fullRound rcs ins = {
  MVmult2 mds_matrix (map2 sbox (zipWith2 (fun x y { x + y }) rcs ins))
};

def fstMap2 f (a1, a2) = {
  (f a1, a2)
};

def singleRound rcs ins = {
  MVmult2 mds_matrix (fstMap2 sbox (zipWith2 (fun x y { x + y }) rcs ins))
};

def stage1 ins0 = {
  def ins1 = fullRound (roundConstant1, roundConstant2) ins0;
  def ins2 = fullRound (roundConstant3, roundConstant4) ins1;
  def ins3 = fullRound (roundConstant5, roundConstant6) ins2;
  def ins4 = fullRound (roundConstant7, roundConstant8) ins3;
  ins4
};

def stage2 ins0 = {
  def ins1 = singleRound (roundConstant9, roundConstant10) ins0;
  def ins2 = singleRound (roundConstant11, roundConstant12) ins1;
  def ins3 = singleRound (roundConstant13, roundConstant14) ins2;
  def ins4 = singleRound (roundConstant15, roundConstant16) ins3;
  def ins5 = singleRound (roundConstant17, roundConstant18) ins4;
  def ins6 = singleRound (roundConstant19, roundConstant20) ins5;
  def ins7 = singleRound (roundConstant21, roundConstant22) ins6;
  def ins8 = singleRound (roundConstant23, roundConstant24) ins7;
  def ins9 = singleRound (roundConstant25, roundConstant26) ins8;
  def ins10 = singleRound (roundConstant27, roundConstant28) ins9;
  def ins11 = singleRound (roundConstant29, roundConstant30) ins10;
  def ins12 = singleRound (roundConstant31, roundConstant32) ins11;
  def ins13 = singleRound (roundConstant33, roundConstant34) ins12;
  def ins14 = singleRound (roundConstant35, roundConstant36) ins13;
  def ins15 = singleRound (roundConstant37, roundConstant38) ins14;
  def ins16 = singleRound (roundConstant39, roundConstant40) ins15;
  def ins17 = singleRound (roundConstant41, roundConstant42) ins16;
  def ins18 = singleRound (roundConstant43, roundConstant44) ins17;
  def ins19 = singleRound (roundConstant45, roundConstant46) ins18;
  def ins20 = singleRound (roundConstant47, roundConstant48) ins19;
  def ins21 = singleRound (roundConstant49, roundConstant50) ins20;
  def ins22 = singleRound (roundConstant51, roundConstant52) ins21;
  def ins23 = singleRound (roundConstant53, roundConstant54) ins22;
  def ins24 = singleRound (roundConstant55, roundConstant56) ins23;
  def ins25 = singleRound (roundConstant57, roundConstant58) ins24;
  def ins26 = singleRound (roundConstant59, roundConstant60) ins25;
  def ins27 = singleRound (roundConstant61, roundConstant62) ins26;
  def ins28 = singleRound (roundConstant63, roundConstant64) ins27;
  def ins29 = singleRound (roundConstant65, roundConstant66) ins28;
  def ins30 = singleRound (roundConstant67, roundConstant68) ins29;
  def ins31 = singleRound (roundConstant69, roundConstant70) ins30;
  def ins32 = singleRound (roundConstant71, roundConstant72) ins31;
  def ins33 = singleRound (roundConstant73, roundConstant74) ins32;
  def ins34 = singleRound (roundConstant75, roundConstant76) ins33;
  def ins35 = singleRound (roundConstant77, roundConstant78) ins34;
  def ins36 = singleRound (roundConstant79, roundConstant80) ins35;
  def ins37 = singleRound (roundConstant81, roundConstant82) ins36;
  def ins38 = singleRound (roundConstant83, roundConstant84) ins37;
  def ins39 = singleRound (roundConstant85, roundConstant86) ins38;
  def ins40 = singleRound (roundConstant87, roundConstant88) ins39;
  def ins41 = singleRound (roundConstant89, roundConstant90) ins40;
  def ins42 = singleRound (roundConstant91, roundConstant92) ins41;
  def ins43 = singleRound (roundConstant93, roundConstant94) ins42;
  def ins44 = singleRound (roundConstant95, roundConstant96) ins43;
  def ins45 = singleRound (roundConstant97, roundConstant98) ins44;
  def ins46 = singleRound (roundConstant99, roundConstant100) ins45;
  def ins47 = singleRound (roundConstant101, roundConstant102) ins46;
  def ins48 = singleRound (roundConstant103, roundConstant104) ins47;
  def ins49 = singleRound (roundConstant105, roundConstant106) ins48;
  def ins50 = singleRound (roundConstant107, roundConstant108) ins49;
  def ins51 = singleRound (roundConstant109, roundConstant110) ins50;
  def ins52 = singleRound (roundConstant111, roundConstant112) ins51;
  def ins53 = singleRound (roundConstant113, roundConstant114) ins52;
  def ins54 = singleRound (roundConstant115, roundConstant116) ins53;
  def ins55 = singleRound (roundConstant117, roundConstant118) ins54;
  def ins56 = singleRound (roundConstant119, roundConstant120) ins55;
  def ins57 = singleRound (roundConstant121, roundConstant122) ins56;
  def ins58 = singleRound (roundConstant123, roundConstant124) ins57;
  def ins59 = singleRound (roundConstant125, roundConstant126) ins58;
  def ins60 = singleRound (roundConstant127, roundConstant128) ins59;
  def ins61 = singleRound (roundConstant129, roundConstant130) ins60;
  def ins62 = singleRound (roundConstant131, roundConstant132) ins61;
  def ins63 = singleRound (roundConstant133, roundConstant134) ins62;
  def ins64 = singleRound (roundConstant135, roundConstant136) ins63;
  def ins65 = singleRound (roundConstant137, roundConstant138) ins64;
  def ins66 = singleRound (roundConstant139, roundConstant140) ins65;
  def ins67 = singleRound (roundConstant141, roundConstant142) ins66;
  def ins68 = singleRound (roundConstant143, roundConstant144) ins67;
  def ins69 = singleRound (roundConstant145, roundConstant146) ins68;
  def ins70 = singleRound (roundConstant147, roundConstant148) ins69;
  def ins71 = singleRound (roundConstant149, roundConstant150) ins70;
  def ins72 = singleRound (roundConstant151, roundConstant152) ins71;
  def ins73 = singleRound (roundConstant153, roundConstant154) ins72;
  def ins74 = singleRound (roundConstant155, roundConstant156) ins73;
  def ins75 = singleRound (roundConstant157, roundConstant158) ins74;
  def ins76 = singleRound (roundConstant159, roundConstant160) ins75;
  def ins77 = singleRound (roundConstant161, roundConstant162) ins76;
  def ins78 = singleRound (roundConstant163, roundConstant164) ins77;
  def ins79 = singleRound (roundConstant165, roundConstant166) ins78;
  def ins80 = singleRound (roundConstant167, roundConstant168) ins79;
  def ins81 = singleRound (roundConstant169, roundConstant170) ins80;
  def ins82 = singleRound (roundConstant171, roundConstant172) ins81;
  def ins83 = singleRound (roundConstant173, roundConstant174) ins82;
  ins83
};

def stage3 ins0 = {
  def ins1 = fullRound (roundConstant175, roundConstant176) ins0;
  def ins2 = fullRound (roundConstant177, roundConstant178) ins1;
  def ins3 = fullRound (roundConstant179, roundConstant180) ins2;
  def ins4 = fullRound (roundConstant181, roundConstant182) ins3;
  ins4
};

def perm ins = {
  stage3 (stage2 (stage1 ins))
};

// Permutation test
perm (0, 1) = 
  ( 28737017598578492660551111965561958005669018206663753188510438988044326316536
  , 33922164273002366575237304185606496058537068988573151647801538995508554021706);

