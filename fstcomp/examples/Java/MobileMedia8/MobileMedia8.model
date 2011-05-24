NewCompound1 : Media+ [includeSorting] [includeFavourites] [includeCopyPhoto] Derivatives [includePrivacy] :: MobilMedia ;

Media : includePhotoAlbum [includeSmsFeature] [capturePhoto] :: Photo
	| Music
	| includeVideo [captureVideo] [simulatePlayVideo] :: Video ;

Music : includeMMAPI ;

Derivatives : [x_CopyPhotoOrSMS] [x_PhotoAlbumOrMusic] [x_NotPhotoAlbum] [x_notMisc] [x_MusicAndVideo] [x_NotMusic] [x_PhotoAlbumAndVideoOrMusic] [x_CapturePhotoOrVideo] [x_NotPrivacy] [x_SMSOrCapturePhoto] [x_misc] [x_MusicOrVideo] [x_SMSOrCapturePhotoOrCaptureVideo] [x_PhotoAlbumOrMusicOrVideo] [x_CopyPhotoOrSMSOrCapturePhoto] [x_notSimulatePlayVideo] :: _Derivatives ;

%%

x_CopyPhotoOrSMS iff includeCopyPhoto or includeSmsFeature ;
x_PhotoAlbumOrMusic iff includePhotoAlbum or includeMMAPI ;
x_NotPhotoAlbum iff not includePhotoAlbum ;
x_NotMusic iff not includeMMAPI ;
x_misc iff includeMMAPI and includePhotoAlbum or includeMMAPI and includeVideo or includeVideo and includePhotoAlbum ;
x_notMisc iff not x_misc ;
x_PhotoAlbumAndVideoOrMusic iff includeMMAPI and includePhotoAlbum or includePhotoAlbum and includeVideo ;
x_CapturePhotoOrVideo iff capturePhoto or captureVideo ;
x_NotPrivacy iff not includePrivacy ;
x_MusicOrVideo iff includeMMAPI or includeVideo ;
x_SMSOrCapturePhotoOrCaptureVideo iff includeSmsFeature or capturePhoto or captureVideo ;
x_PhotoAlbumOrMusicOrVideo iff includePhotoAlbum or includeMMAPI or includeVideo ;
x_CopyPhotoOrSMSOrCapturePhoto iff includeCopyPhoto or includeSmsFeature or capturePhoto ;
x_notSimulatePlayVideo iff not simulatePlayVideo ;
x_MusicAndVideo iff includeMMAPI and includeVideo ;
x_SMSOrCapturePhoto iff includeSmsFeature or capturePhoto;

