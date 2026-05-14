<?php

namespace Cloudflare\APO\API\Exception;

class ZoneSettingFailException extends CloudFlareException
{
    protected $message = 'Oops, something went wrong, please try again in a few minutes';

    public function __construct($message = null, $code = 0, ?\Throwable $previous = null)
    {
        if ($message !== null) {
            $this->message = $message;
        }
        parent::__construct($this->message, $code, $previous);
    }
}
